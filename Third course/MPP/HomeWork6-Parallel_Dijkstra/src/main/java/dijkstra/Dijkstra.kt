package dijkstra

import java.util.*
import java.util.concurrent.Phaser
import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.thread

private val NODE_DISTANCE_COMPARATOR = Comparator<Node> { o1, o2 -> o1!!.distance.compareTo(o2!!.distance) }

// Returns `Integer.MAX_VALUE` if a path has not been found.
fun shortestPathParallel(start: Node) {
    val workers = Runtime.getRuntime().availableProcessors()
    // The distance to the start node is `0`
    start.distance = 0
    // Create a priority (by distance) queue and add the start node into it
    val q = PriorityMultiQueue(workers, NODE_DISTANCE_COMPARATOR)
    q.push(start)
    // Run worker threads and wait until the total work is done
    val onFinish = Phaser(workers + 1) // `arrive()` should be invoked at the end by each worker
    val activeNodes = AtomicInteger(1)
    repeat(workers) {
        thread {
            while (activeNodes.get() > 0) {
                val cur: Node = q.pop() ?: continue

                for (e in cur.outgoingEdges) {
                    while (true) {
                        val old = e.to.distance
                        val new = cur.distance + e.weight

                        if (new < old) {
                            if (e.to.casDistance(old, new)) {
                                q.push(e.to)
                                activeNodes.incrementAndGet()
                                break
                            }
                        } else {
                            break
                        }
                    }
                }
                activeNodes.decrementAndGet()
            }
            onFinish.arrive()
        }
    }
    onFinish.arriveAndAwaitAdvance()
}


class PriorityMultiQueue(private val numberOfQueues: Int, private val comparator: Comparator<Node>) {
    private val queueList: List<PriorityBlockingQueue<Node>> =
        Collections.nCopies(numberOfQueues * 2, PriorityBlockingQueue(comparator))
    private val randomQueue: PriorityBlockingQueue<Node>
        get() = queueList[ThreadLocalRandom.current().nextInt(numberOfQueues)]

    fun pop(): Node {
        val queue1 = randomQueue
        val queue2 = randomQueue
        queue1.lock()
        queue2.lock()
        val result: Node = try {
            if (Comparator.nullsLast(comparator).compare(queue1.peek(), queue2.peek()) > 0) {
                queue2.pop()
            } else {
                queue1.pop()
            }
        } finally {
            queue1.unlock()
            queue2.unlock()
        }
        return result
    }

    fun push(node: Node): Boolean {
        val queue = randomQueue
        queue.lock()
        val result: Boolean = try {
            queue.push(node)
        } finally {
            queue.unlock()
        }
        return result
    }

}

internal class PriorityBlockingQueue<T>(comparator: Comparator<T>?) {
    private val lock: Lock
    private val queue: Queue<T>
    fun pop(): T {
        return queue.poll()
    }

    fun push(value: T): Boolean {
        return queue.add(value)
    }

    fun peek(): T {
        return queue.peek()
    }

    fun lock() {
        lock.lock()
    }

    fun unlock() {
        lock.unlock()
    }

    init {
        lock = ReentrantLock()
        queue = PriorityQueue(comparator)
    }
}