package stack;

import kotlinx.atomicfu.AtomicArray;
import kotlinx.atomicfu.AtomicRef;

import java.util.concurrent.ThreadLocalRandom;

public class StackImpl implements Stack {
    private static class Node {
        final AtomicRef<Node> next;
        final int x;

        Node(int x, Node next) {
            this.next = new AtomicRef<>(next);
            this.x = x;
        }
    }

    private final AtomicRef<Node> head = new AtomicRef<>(null);
    private static final int CAPACITY = Runtime.getRuntime().availableProcessors() * 2;
    private static final int TIMEOUT_IN_ITERATIONS = CAPACITY / 2;
    EliminationArray eliminationArray = new EliminationArray(CAPACITY, TIMEOUT_IN_ITERATIONS);

    @Override
    public int pop() {
        Integer value = eliminationArray.visit(null, CAPACITY);
        if (value != null) {
            return value;
        } else {
            while (true) {  // CAS loop
                Node oldHead = head.getValue();
                if (oldHead == null) {
                    return Integer.MIN_VALUE;
                }
                if (head.compareAndSet(oldHead, oldHead.next.getValue())) {
                    return oldHead.x;
                }
            }
        }
    }

    @Override
    public void push(int x) {
        Integer value = eliminationArray.visit(x, CAPACITY);
        if (value != null) {
            Node newHead;
            while (true) {  // CAS loop
                Node oldHead = head.getValue();
                newHead = new Node(value, oldHead);
                if (head.compareAndSet(oldHead, newHead)) {
                    return;
                }
            }
        }
    }
}

class EliminationArray {
    private final long timeoutInIterations;
    private final AtomicArray<Integer> exchanger;
    private final int size;


    public EliminationArray(final int capacity, final int timeoutInIterations) {
        size = capacity;
        this.timeoutInIterations = timeoutInIterations;
        exchanger = new AtomicArray<>(size);
        for (int i = 0; i < size; i++) {
            exchanger.get(i).setValue(null);
        }
    }

    public Integer visit(Integer value, int range) {
        int index = ThreadLocalRandom.current().nextInt(range);
        if (value == null) {
            for (int iter = 0; iter < timeoutInIterations; iter++, index = (index + 1) % size) {
                AtomicRef<Integer> slot = exchanger.get(index);
                Integer valueOfSLot = slot.getValue();
                if (valueOfSLot != null && slot.compareAndSet(valueOfSLot, null)) {
                    return valueOfSLot;
                }
            }
        } else {
            for (int iter = 0; iter < timeoutInIterations; iter++, index = (index + 1) % size) {
                AtomicRef<Integer> slot = exchanger.get(index);
                if (slot.compareAndSet(null, value)) {
                    for (int iter2 = 0; iter2 < timeoutInIterations + iter; iter2++) {
                        Integer temp = slot.getValue();
                        if (temp == null) {
                            return null;
                        }
                    }
                    if (slot.compareAndSet(value, null)) {
                        return value;
                    } else {
                        return null;
                    }
                }
            }
        }
        return value;
    }
}
