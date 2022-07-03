import system.MergerEnvironment

class MergerImpl<T : Comparable<T>>(
    private val mergerEnvironment: MergerEnvironment<T>,
    prevStepBatches: Map<Int, List<T>>?
) : Merger<T> {
    private val prevStepBatches: Map<Int, List<T>>? = prevStepBatches
    private val batches: MutableMap<Int, List<T>> = getBatches()

    override fun mergeStep(): T? {
        val min = findMinValue(batches)

        return if (min != null) {
            val keys: List<T> = batches[min.first].orEmpty().drop(1)
            update(keys, min.first)
            min.second
        } else {
            null
        }
    }

    override fun getRemainingBatches(): Map<Int, List<T>> {
        return batches
    }

    private fun getBatches(): MutableMap<Int, List<T>> {
        return if (prevStepBatches != null) {
            prevStepBatches.toMutableMap()
        } else {
            val result = mutableMapOf<Int, List<T>>()
            for (i in 0 until mergerEnvironment.dataHoldersCount) {
                result[i] = mergerEnvironment.requestBatch(i)
            }
            result
        }
    }

    private fun findMinValue(map: Map<Int, List<T>>): Pair<Int, T>? {
        val result = map.minByOrNull { e ->
            e.value.first()
        }

        return if (result == null) {
            null
        } else {
            Pair(result.key, result.value.first())
        }
    }

    private fun update(keys: List<T>, index: Int) {
        if (keys.isEmpty()) {
            val newKeys = mergerEnvironment.requestBatch(index)
            if (newKeys.isEmpty()) {
                batches.remove(index)
            } else {
                batches[index] = newKeys
            }
        } else {
            batches[index] = keys
        }
    }
}