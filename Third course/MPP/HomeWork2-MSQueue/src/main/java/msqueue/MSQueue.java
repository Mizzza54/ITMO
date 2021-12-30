package msqueue;

import kotlinx.atomicfu.AtomicRef;

public class MSQueue implements Queue {
    private final AtomicRef<Node> head;
    private final AtomicRef<Node> tail;

    public MSQueue() {
        Node dummy = new Node(0, null);
        this.head = new AtomicRef<>(dummy);
        this.tail = new AtomicRef<>(dummy);
    }

    @Override
    public void enqueue(int x) {
        Node newTail = new Node(x, null);
        while (true) {
            Node curTail = tail.getValue();
            if (curTail.next.compareAndSet(null, newTail)) {
                tail.compareAndSet(curTail, newTail);
                return;
            } else {
                tail.compareAndSet(curTail, curTail.next.getValue());
            }
        }
    }

    @Override
    public int dequeue() {
        while (true) {                                // Заходим в цикл
            Node curHead = head.getValue();           // Читаем голову
            Node curTail = tail.getValue();           // Читаем хвост
            Node next = curHead.next.getValue();      // Читаем head->next
            if (next == null) {
                return Integer.MIN_VALUE;
            }
            if (curHead == curTail) {                 // Проверка на инвариант
                tail.compareAndSet(curTail, next);
            } else {
                if (head.compareAndSet(curHead, next)) {
                    return next.x;
                }
            }
        }
    }

    @Override
    public int peek() {
        Node next = head.getValue().next.getValue();
        return next == null ? Integer.MIN_VALUE : next.x;
    }

    private static class Node {
        final int x;
        final AtomicRef<Node> next;

        Node(int x, Node next) {
            this.x = x;
            this.next = new AtomicRef<>(next);
        }
    }
}