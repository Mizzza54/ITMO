class ConsistentHashImpl<K> : ConsistentHash<K> {
    private var vnodes: MutableList<VNode> = mutableListOf()
    private var countShard: Int = 0

    override fun getShardByKey(key: K): Shard {
        return if (key.hashCode() > vnodes.last().hashcode) {
            vnodes[0].shard
        } else {
            val pos = vnodes.binarySearch { it.hashcode.compareTo(key.hashCode()) }
            if (pos >= 0) {
                vnodes[pos].shard
            } else {
                vnodes[-pos - 1].shard
            }
        }
    }

    override fun addShard(newShard: Shard, vnodeHashes: Set<Int>): Map<Shard, Set<HashRange>> {
        countShard++
        vnodes.addAll(vnodeHashes.map { VNode(newShard, it) })
        vnodes.sort()
        return circumambulation(newShard)
    }

    override fun removeShard(shard: Shard): Map<Shard, Set<HashRange>> {
        val ans = circumambulation(shard)
        countShard--
        vnodes.removeAll { it.shard == shard }
        return ans
    }

    private fun circumambulation(shard: Shard): MutableMap<Shard, MutableSet<HashRange>> {
        val result: MutableMap<Shard, MutableSet<HashRange>> = mutableMapOf()

        if (countShard < 2) {
            return result
        }

        val start = vnodes.indexOfFirst { it.shard != shard }
        var prev: VNode = vnodes[start]

        for (i in 0 until vnodes.size) {
            val index = (start + i) % vnodes.size
            val cur = vnodes[index]
            val next = vnodes[if (index + 1 == vnodes.size) 0 else index + 1]

            if (cur.shard == shard) {
                if (next.shard != shard) {
                    result.putIfAbsent(next.shard, mutableSetOf())
                    result[next.shard]!!.add(HashRange(prev.hashcode + 1, cur.hashcode))
                }
            } else {
                prev = cur
            }
        }

        return result
    }
}

data class VNode(val shard: Shard, val hashcode: Int) : Comparable<VNode> {
    override fun compareTo(other: VNode): Int {
        return this.hashcode.compareTo(other.hashcode)
    }
}