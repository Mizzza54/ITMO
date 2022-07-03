package mutex

/**
 * Distributed mutual exclusion implementation.
 * All functions are called from the single main thread.
 *
 * @author Michael Gerasimov
 */
class ProcessImpl(private val env: Environment) : Process {

    private var status = Status.WAIT
    private val fork = BooleanArray(env.nProcesses + 1) { f -> f < env.processId }
    private val isDirty = BooleanArray(env.nProcesses + 1) { _ -> true }

    private val isWantFork = BooleanArray(env.nProcesses + 1)


    override fun onMessage(srcId: Int, message: Message) {
        message.parse {
            val type = readEnum<MsgType>()
            when(type) {
                MsgType.REQ -> {
                    isWantFork[srcId] = true
                    releaseFork(srcId)
                    requestFork(srcId)
                }
                MsgType.OK -> {
                    fork[srcId] = true
                }
            }
            if (status == Status.REQ && canEat()) {
                eat()
            }
        }
    }

    override fun onLockRequest() {
        status = Status.REQ
        if (canEat()) {
            eat()
        } else {
            for (i in 1..env.nProcesses) {
                if (i != env.processId) {
                    requestFork(i)
                }
            }
        }
    }

    override fun onUnlockRequest() {
        status = Status.WAIT
        env.unlocked()
        for (i in 1..env.nProcesses) {
            if (i != env.processId) {
                releaseFork(i)
            }
        }
    }

    fun requestFork(srcId: Int) {
        if (status == Status.REQ && !fork[srcId]) {
            env.send(srcId) {
                writeEnum(MsgType.REQ)
            }

        }
    }

    fun releaseFork(srcId: Int) {
        if (status != Status.CS && isDirty[srcId] && isWantFork[srcId]) {
            isDirty[srcId] = false
            fork[srcId] = false
            env.send(srcId) {
                writeEnum(MsgType.OK)
            }
            isWantFork[srcId] = false
        }
    }

    fun canEat(): Boolean {
        for (i in 1..env.nProcesses) {
            if (i != env.processId) {
                if (!fork[i]) {
                    return false
                }
            }
        }
        return true
    }

    fun eat() {
        env.locked()
        status = Status.CS
        for (i in 1..env.nProcesses) {
            if (i != env.processId) {
                isDirty[i] = true
            }
        }
    }

    enum class Status { WAIT, REQ, CS }
    enum class MsgType { REQ, OK }
}