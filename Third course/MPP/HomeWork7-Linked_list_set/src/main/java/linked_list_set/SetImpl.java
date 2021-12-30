package linked_list_set;

import kotlinx.atomicfu.AtomicRef;

public class SetImpl implements Set {

    interface AbstractNode {
        int getValue();
        AtomicRef<AbstractNode> getNext();
    }

    private static class Removed implements AbstractNode {
        private final Node node;

        private Removed(Node node) {
            this.node = node;
        }


        @Override
        public int getValue() {
            return node.getValue();
        }

        @Override
        public AtomicRef<AbstractNode> getNext() {
            return node.getNext();
        }
    }

    private static class Node implements AbstractNode {
        AtomicRef<AbstractNode> next;
        int x;

        Node(int x, AbstractNode next) {
            this.next = new AtomicRef<>(next);
            this.x = x;
        }

        @Override
        public int getValue() {
            return x;
        }

        @Override
        public AtomicRef<AbstractNode> getNext() {
            return next;
        }
    }

    private static class Window {
        AbstractNode cur, next;
    }

    private final AtomicRef<Node> head = new AtomicRef<>(new Node(Integer.MIN_VALUE, new Node(Integer.MAX_VALUE, null)));

    /**
     * Returns the {@link Window}, where cur.x < x <= next.x
     */
    private Window findWindow(int x) {
        Window w = new Window();

        retry:
        while (true) {
            w.cur = head.getValue();
            w.next = w.cur.getNext().getValue();
            while (w.next.getValue() < x) {
                AbstractNode node = w.next.getNext().getValue();
                if (node instanceof Removed) {
                    if (!w.cur.getNext().compareAndSet(w.next, ((Removed) node).node)) {
                        continue retry;
                    }
                    w.next = ((Removed) node).node;
                } else {
                    w.cur = w.next;
                    w.next = node;
                }
            }

            AbstractNode node = w.next.getNext().getValue();
            if (node instanceof Removed) {
                if (!w.cur.getNext().compareAndSet(w.next, ((Removed) node).node)) {
                    continue;
                }
                w.next = ((Removed) node).node;
            }
            return w;
        }
    }

    @Override
    public boolean add(int x) {
        while (true) {
            Window w = findWindow(x);
            if (w.next.getValue() == x) {
                return false;
            }

            AbstractNode node = new Node(x, w.next);
            if (w.cur.getNext().compareAndSet(w.next, node)) {
                return true;
            }
        }
    }

    @Override
    public boolean remove(int x) {
        while (true) {
            Window w = findWindow(x);
            if (w.next.getValue() != x) {
                return false;
            }

            AbstractNode node = w.next.getNext().getValue();

            if (node instanceof Removed) {
                return false;
            }

            if (w.next.getNext().compareAndSet(node, new Removed((Node) node))) {
                w.cur.getNext().compareAndSet(w.next, node);
                return true;
            }
        }
    }

    @Override
    public boolean contains(int x) {
        Window w = findWindow(x);
        return w.next.getValue() == x;
    }
}