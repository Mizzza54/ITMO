package raft

import raft.Message.*

/**
 * Raft algorithm implementation.
 * All functions are called from the single main thread.
 *
 * @author Michael Gerasimov
 */
class ProcessImpl(private val env: Environment) : Process {
    private val storage = env.storage
    private val machine = env.machine
    private val majority: Int = env.nProcesses / 2 + 1
    private val commandQueue = ArrayDeque<Command>()

    private var state: State = State.FOLLOWER
    private var currentTerm: Int = storage.readPersistentState().currentTerm
    private var votedFor: Int? = storage.readPersistentState().votedFor
    private var leaderId: Int? = null
    private var commitIndex = 0
    private var votesGranted: Int = 0
    private var nextIndex = Array(env.nProcesses) { _ -> commitIndex + 1 }
    private var matchIndex = Array(env.nProcesses) { _ -> 0 }
    private var lastApplied = 0

    init {
        env.startTimeout(Timeout.ELECTION_TIMEOUT)
    }

    override fun onTimeout() {
        when (state) {
            State.FOLLOWER, State.CANDIDATE -> {
                state = State.CANDIDATE
                currentTerm++
                votedFor = env.processId
                votesGranted = 1
                writePersistentState()
                `send a message to everyone except me` { _: Int -> RequestVoteRpc(currentTerm, getLastLogId()) }
                env.startTimeout(Timeout.ELECTION_TIMEOUT)
            }
            State.LEADER -> {
                `send a message to everyone except me` { i: Int ->
                    val entry: LogEntry? = if (getLastLogIndex() >= nextIndex[i - 1]) {
                        storage.readLog(nextIndex[i - 1])
                    } else {
                        null
                    }

                    val message =
                        AppendEntryRpc(
                            currentTerm,
                            storage.readLog(nextIndex[i - 1] - 1)?.id ?: START_LOG_ID,
                            commitIndex,
                            entry
                        )
                    message
                }

                while (!commandQueue.isEmpty()) {
                    onClientCommand(commandQueue.removeFirst())
                }

                env.startTimeout(Timeout.LEADER_HEARTBEAT_PERIOD)
            }
        }
    }

    override fun onMessage(srcId: Int, message: Message) {
        processingDeprecatedTerm(message.term)
        writePersistentState()

        when (message) {
            is AppendEntryResult -> processingAppendEntryResult(srcId, message)
            is AppendEntryRpc -> processingAppendEntryRpc(srcId, message)
            is ClientCommandResult -> processingClientCommandResult(srcId, message)
            is ClientCommandRpc -> processingClientCommandRpc(message)
            is RequestVoteResult -> processingRequestVoteResult(message)
            is RequestVoteRpc -> processingRequestVoteRpc(srcId, message)
        }

        while (lastApplied < commitIndex && lastApplied < storage.readLastLogId().index) {
            lastApplied++
            val command = storage.readLog(lastApplied)!!.command
            val commandResult = machine.apply(command)

            if (command.processId == env.processId) {
                env.onClientCommandResult(commandResult)
            } else {
                env.send(command.processId, ClientCommandResult(currentTerm, commandResult))
            }
        }
    }

    override fun onClientCommand(command: Command) {
        when (state) {
            State.FOLLOWER -> {
                if (leaderId == null) {
                    commandQueue.add(command)
                } else {
                    env.send(leaderId!!, ClientCommandRpc(currentTerm, command))
                }
            }
            State.CANDIDATE -> {
                commandQueue.add(command)
            }
            State.LEADER -> {
                val entry = LogEntry(LogId(getLastLogIndex() + 1, currentTerm), command)
                storage.appendLogEntry(entry)
                `send a message to everyone except me` { i: Int ->
                    if (nextIndex[i - 1] == entry.id.index) {
                        AppendEntryRpc(
                            currentTerm,
                            storage.readLog(nextIndex[i - 1] - 1)?.id ?: START_LOG_ID,
                            commitIndex,
                            entry
                        )
                    } else {
                        null
                    }
                }
            }
        }
    }

    private fun processingAppendEntryRpc(srcId: Int, message: AppendEntryRpc) {
        if (message.term < currentTerm) {
            env.send(srcId, AppendEntryResult(currentTerm, null))
            return
        }

        leaderId = srcId
        if (srcId != env.processId) {
            state = State.FOLLOWER
        }

        while (!commandQueue.isEmpty()) {
            env.send(srcId, ClientCommandRpc(currentTerm, commandQueue.removeFirst()))
        }

        if (message.leaderCommit > commitIndex) {
            commitIndex = minOf(message.leaderCommit, getLastLogIndex())
        }

        val success = message.prevLogId == START_LOG_ID || (
                storage.readLog(message.prevLogId.index) != null
                        && storage.readLog(message.prevLogId.index)!!.id.term == message.prevLogId.term
                )

        if (success && message.entry != null) {
            storage.appendLogEntry(message.entry)
            env.send(srcId, AppendEntryResult(currentTerm, message.entry.id.index))
        } else if (success) {
            env.send(srcId, AppendEntryResult(currentTerm, message.prevLogId.index))
        } else {
            env.send(srcId, AppendEntryResult(currentTerm, null))
        }

        while (lastApplied < commitIndex && lastApplied < storage.readLastLogId().index) {
            lastApplied++
            machine.apply(storage.readLog(lastApplied)!!.command)
        }

        env.startTimeout(Timeout.ELECTION_TIMEOUT)
    }

    private fun processingAppendEntryResult(srcId: Int, message: AppendEntryResult) {
        if (message.term < currentTerm) {
            return
        }

        if (message.lastIndex != null) {
            matchIndex[srcId - 1] = message.lastIndex
            nextIndex[srcId - 1] = message.lastIndex + 1

            var prev = commitIndex
            while (++prev <= storage.readLastLogId().index) {
                if (prev > storage.readLastLogId().index || storage.readLog(prev)!!.id.term != currentTerm) {
                    continue
                }

                val count = matchIndex.count() {it >= prev} + 1

                if (count >= majority) {
                    commitIndex = prev
                } else {
                    break
                }
            }

            var command: Command? = null
            var commandResult: CommandResult? = null
            while (lastApplied < commitIndex) {
                lastApplied++
                command = storage.readLog(lastApplied)!!.command
                commandResult = machine.apply(command)
            }

            if (command != null && commandResult != null) {
                if (command.processId == env.processId) {
                    env.onClientCommandResult(commandResult)
                } else {
                    env.send(command.processId, ClientCommandResult(currentTerm, commandResult))
                }
            }
        } else {
            nextIndex[srcId - 1] -= 1
            env.send(
                srcId,
                AppendEntryRpc(
                    currentTerm,
                    storage.readLog(nextIndex[srcId - 1] - 1)?.id ?: START_LOG_ID,
                    commitIndex,
                    storage.readLog(nextIndex[srcId - 1])
                )
            )
        }
    }

    private fun processingRequestVoteRpc(srcId: Int, message: RequestVoteRpc) {
        val result: Boolean = if (message.term < currentTerm) {
            false
        } else if (votedFor == null && validationLog(message.lastLogId)) {
            env.startTimeout(Timeout.ELECTION_TIMEOUT)
            votedFor = srcId
            writePersistentState()
            true
        } else if (votedFor != null) {
            srcId == votedFor
        } else {
            if (state != State.CANDIDATE) {
                env.startTimeout(Timeout.ELECTION_TIMEOUT)
            }
            false
        }

        env.send(srcId, RequestVoteResult(currentTerm, result))
    }

    private fun processingRequestVoteResult(message: RequestVoteResult) {
        if (message.term < currentTerm) {
            return
        }

        if (message.voteGranted) {
            votesGranted++
        }

        if (state == State.CANDIDATE && votesGranted >= majority) {
            state = State.LEADER
            nextIndex = Array(env.nProcesses) { _ -> getLastLogIndex() + 1 }
            matchIndex = Array(env.nProcesses) { _ -> 0 }

            `send a message to everyone except me` { i: Int ->
                AppendEntryRpc(
                    currentTerm,
                    storage.readLog(nextIndex[i - 1] - 1)?.id ?: START_LOG_ID,
                    commitIndex,
                    null
                )
            }
            env.startTimeout(Timeout.LEADER_HEARTBEAT_PERIOD)
        }
    }

    private fun processingClientCommandRpc(message: ClientCommandRpc) {
        onClientCommand(message.command)
    }

    private fun processingClientCommandResult(srcId: Int, message: ClientCommandResult) {
        if (leaderId == null) {
            leaderId = srcId
        }

        while (!commandQueue.isEmpty()) {
            env.send(srcId, ClientCommandRpc(currentTerm, commandQueue.removeFirst()))
        }

        env.onClientCommandResult(message.result)
    }

    private fun `send a message to everyone except me`(factoryMessage: (i: Int) -> Message?) {
        for (i in 1..env.nProcesses) {
            if (i == env.processId) {
                continue
            }
            val message: Message? = factoryMessage(i)

            if (message != null) {
                env.send(i, message)
            }
        }
    }

    private fun processingDeprecatedTerm(term: Int): Boolean {
        return if (term > currentTerm) {
            currentTerm = term
            state = State.FOLLOWER
            votedFor = null
            leaderId = null
            env.startTimeout(Timeout.ELECTION_TIMEOUT)
            true
        } else {
            false
        }
    }

    private fun validationLog(lastLogId: LogId): Boolean {
        val leaderLogLastTermIsBigger = if (getLastLogTerm() == START_LOG_ID.term) {
            false
        } else {
            getLastLogTerm() < lastLogId.term
        }

        if (leaderLogLastTermIsBigger) {
            return true
        }

        val leaderLogLastTermEqual = if (getLastLogTerm() == START_LOG_ID.term) {
            true
        } else {
            getLastLogTerm() == lastLogId.term
        }

        val leaderLogLongerOrEqual = getLastLogIndex() <= lastLogId.index

        return leaderLogLastTermEqual && leaderLogLongerOrEqual
    }

    private fun getLastLogId(): LogId {
        return storage.readLastLogId()
    }

    private fun getLastLogIndex(): Int {
        return storage.readLastLogId().index
    }

    private fun getLastLogTerm(): Int {
        return storage.readLastLogId().term
    }

    private fun writePersistentState() {
        storage.writePersistentState(PersistentState(currentTerm, votedFor))
    }
}

enum class State {
    FOLLOWER, CANDIDATE, LEADER
}

