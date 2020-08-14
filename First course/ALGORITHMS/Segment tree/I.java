import java.util.Scanner;

/**
  @author Michale Gerasimov
  start: 24.02.2020
  @version -
 **/

public class I {
    public static class Node {
        public int l, r, count;
        public long max = 0, total = 0;
        public boolean flag = true;
        public Node left = null, right = null;

        Node(int l, int r, int count, long max, long total, Node left, Node right, boolean flag) {
            this.l= l;
            this.r = r;
            this.count = count;
            this.max = max;
            this.total = total;
            this.left = left;
            this.right = right;
            this.flag = flag;
        }
    }

    public static Scanner scanner = new Scanner(System.in);

    public static void main(String[] args) {
        int n, a, b;
        long v;
        String command;
        n = scanner.nextInt();
        Node root = new Node(0, n - 1, n, 0, 0, null, null, true);
        do {
            command = scanner.next();
            switch (command) {
                case "Q":
                    v = scanner.nextLong();
                    System.out.println(sum(root, v));
                    break;
                case "I":
                    a = scanner.nextInt();
                    b = scanner.nextInt();
                    v = scanner.nextLong();
                    set(root, a - 1, b - 1, v);
            }
        } while (!command.equals("E"));
    }

    public static void push (Node v) {
        if (v.left == null && v.right == null) {
            v.left = new Node(v.l, (v.r + v.l) / 2, (v.r + v.l) / 2 - v.l + 1, 0, 0, null, null, true);
            v.right = new Node( (v.r + v.l) / 2 + 1, v.r, v.r - ((v.r + v.l) / 2 + 1) + 1, 0, 0, null, null, true);
        }
        if (v.flag) {
            (v.left).max = (v.left).total = v.total * (v.left).count / v.count;
            (v.right).max = (v.right).total = v.total - (v.left).total;
            v.flag = false;
            (v.left).flag = true;
            (v.right).flag = true;
        }
        return;
    }
    
    public static void set(Node v, int a, int b, long value) {
        if (a > b) {
            return;
        }

        if (v.l == a && v.r == b) {
            v.flag = true;
            v.max = v.total = value * (v.r - v.l + 1);
            return;
        }

        push(v);

        int cm = (v.l + v.r) / 2;
        set(v.left, a, Math.min(b, cm), value);
        set(v.right, Math.max(a, cm + 1), b, value);

        v.total = (v.left).total + (v.right).total;
        v.max = Math.max((v.left).max, (v.left).total + (v.right).max);
    }

    public static long sum(Node v, long h) {
        if (v.r == v.l) {
            if (v.max <= h) {
                return v.l + 1;
            }
            return v.l;
        }
        push(v);
        if ((v.left).max > h) {
            return sum((v.left), h);
        }

        return sum((v.right), h - (v.left).total);
    }
}
