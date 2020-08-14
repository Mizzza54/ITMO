package queue;

/**
 * @author Michale Gerasimov
 * start: 03.03.2020
 * @version -
 */

public class LinkedQueue extends AbstractQueue {

    private class Node {
        private Node next;
        private Object value;

        Node(Object value) {
            assert value != null;

            this.value = value;
            this.next = null;
        }
    }

    private Node head, tail;

    protected void enqueueImpl(Object element) {
        if (size == 0) {
            head = new Node(element);
            tail = head;
        } else {
            tail.next = new Node(element);
            tail = tail.next;
        }
        tail.next = null;
    }

    protected void dequeueImpl() {
        if (size == 1) {
            head = null;
            tail = null;
        } else {
            head = head.next;
        }
    }

    protected void clearImpl() {
        head = null;
        tail = null;
    }

    protected Object peekImpl() {
        return tail.value;
    }

    protected Object elementImpl() {
        return head.value;
    }

}

