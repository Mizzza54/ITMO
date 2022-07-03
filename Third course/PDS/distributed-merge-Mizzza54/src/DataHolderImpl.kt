import system.DataHolderEnvironment

class DataHolderImpl<T : Comparable<T>>(
    private val keys: List<T>,
    private val dataHolderEnvironment: DataHolderEnvironment
) : DataHolder<T> {
    private var checkpoint: Int = 0
    private var current: Int = 0

    override fun checkpoint() {
        checkpoint = current
    }

    override fun rollBack() {
        current = checkpoint
    }

    override fun getBatch(): List<T> {
        return if (keys.size - current < dataHolderEnvironment.batchSize) {
            val prev = current
            current = keys.size
            keys.subList(prev, keys.size)
        } else {
            current += dataHolderEnvironment.batchSize
            keys.subList(current - dataHolderEnvironment.batchSize, current)
        }
    }
}