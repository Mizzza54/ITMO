package dijkstra.messages

sealed class Message

data class MessageDistance(val distance : Long, val edgeDistance : Long) : Message()

object MessageAck : Message()

object MessageIAmYourNewChild : Message()

object MessageIAmNotYourChildAnymore : Message()
