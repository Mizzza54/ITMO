package dijkstra

import dijkstra.messages.*
import dijkstra.system.environment.Environment

class ProcessImpl(private val environment: Environment) : Process {

    private var distance: Long? = null
    private var childCount: Int = 0
    private var balance: Int = 0
    private var parentId: Int = -1

    private fun isGreen(): Boolean {
        return childCount == 0 && balance == 0
    }

    private fun sendMessageToAllNeighbours() {
        for (elem in environment.neighbours) {
            val key = elem.key
            val value = elem.value
            sendMessage(key, MessageDistance(distance!!, value))
        }
        deleteFromTree()
    }

    private fun deleteFromTree() {
        if (isGreen()) {
            if (parentId == -1) {
                environment.finishExecution()
            } else {
                sendMessage(parentId, MessageIAmNotYourChildAnymore)
                parentId = -1
            }
        }
    }

    override fun onMessage(srcId: Int, message: Message) {
        when(message) {
            is MessageDistance -> {
                val messageDistance = message.distance
                val edgeDistance = message.edgeDistance
                if (distance == null || messageDistance + edgeDistance < distance!!) {
                    if (parentId != -1) {
                        sendMessage(parentId, MessageIAmNotYourChildAnymore)
                    }
                    parentId = srcId
                    sendMessage(parentId, MessageIAmYourNewChild)
                    distance = messageDistance + edgeDistance
                    sendMessageToAllNeighbours()
                }
                sendMessage(srcId, MessageAck)
                return
            }
            is MessageAck -> {
                balance--
            }
            is MessageIAmYourNewChild -> {
                childCount++
            }
            is MessageIAmNotYourChildAnymore -> {
                childCount--
            }
        }
        deleteFromTree()
    }

    override fun getDistance(): Long? {
        return distance
    }

    override fun startComputation() {
        distance = 0
        sendMessageToAllNeighbours()
    }

    private fun sendMessage(dstId: Int, message: Message) {
        environment.send(dstId, message)
        if (message is MessageDistance) {
            balance++
        }
    }
}
