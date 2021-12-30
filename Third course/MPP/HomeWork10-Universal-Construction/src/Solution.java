/**
 * @author Gerasimov Michael
 */
public class Solution implements AtomicCounter {

    private final Node root = new Node(0);
    private final ThreadLocal<Node> last = ThreadLocal.<Node>withInitial(()-> {return root;});

    public int getAndAdd(final int x) {
        Node node;
        int old, upd;
        do {
            old = last.get().getValue();
            upd = old + x;
            node = new Node(upd);
            last.set(last.get().getNext().decide(node));
        } while (node != last.get());
        return old;
    }

    private static class Node {
        private final int value;
        private final Consensus<Node> next = new Consensus<>();

        public Node(final int value) {
            this.value = value;
        }

        public int getValue() {
            return value;
        }

        public Consensus<Node> getNext() {
            return next;
        }
    }
}
