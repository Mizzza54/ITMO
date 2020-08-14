import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 05.04.2020
 * @version -
 */

public class D {
    public static Random random = new Random();
    public static Treap MainRoot;
    public static Queue<Treap> MainQueue = new LinkedList<>();

    public static void main(String[] args) {
        FastReader scanner = new FastReader(System.in);
        long n = scanner.nextLong();
        boolean plus = false;
        String line;
        long prev = 0;

        for (long i = 0; i < n; i++) {
            line = scanner.next();
            switch (line) {
                case "+":
                    long key = scanner.nextLong();
                    if (plus) {
                        insert(MainRoot, ((key + prev) % 1000000000));
                    } else {
                        insert(MainRoot, key);
                    }
                    plus = false;
                    break;
                case "?":
                    plus = true;
                    long left = scanner.nextLong();
                    long right = scanner.nextLong();
                    prev = SumOnSegment(MainRoot, left, right);
                    System.out.println(prev);
                    break;
            }
        }

        //PrintTree(MainRoot);
    }

    public static class Pair {
        Treap left;
        Treap right;

        Pair (Treap left, Treap right) {
            this.left = left;
            this.right = right;
        }
    }

    public static class Treap {
        public long key, priority, sum, size;
        public Treap left, right;

        Treap (long key, Treap left, Treap right) {
            this.key = key;
            this.priority = random.nextLong();
            this.left = left;
            this.right = right;
            this.sum = key;
            this.size = 1;
        }
    }

    public static Treap merge(Treap L, Treap R) {
        if (L == null) {
            update(R);
            return R;
        }
        if (R == null) {
            update(L);
            return L;
        }
        if (L.priority > R.priority) {
            L.right = merge(L.right, R);
            update(L);
            return L;
        } else {
            R.left = merge(L, R.left);
            update(R);
            return  R;
        }
    }

    public static Pair split(Treap T, long key) {
        if (T == null) {
            return new Pair(null, null);
        } else if (key >= T.key) {
            Pair pair = split(T.right, key);
            T.right = pair.left;
            update(T);
            return new Pair(T, pair.right);
        } else {
            Pair pair = split(T.left, key);
            T.left = pair.right;
            update(T);
            return new Pair(pair.left, T);
        }
    }

    public static long getSum(Treap T) {
        if (T == null) {
            return  0;
        } else {
            return T.sum;
        }
    }

    public static long getSize(Treap T) {
        if (T == null) {
            return  0;
        } else {
            return T.size;
        }
    }

    public static void update(Treap T) {
        if (T == null) {
            return;
        }
        T.sum = 0;
        T.sum += getSum(T.left) + getSum(T.right) + T.key;
        T.size = 1;
        T.size = T.size + getSize(T.left) + getSize(T.right);
    }

    public static void insert(Treap T, long key) {
        if (search(MainRoot, key) != null) {
            return;
        }
        Pair pair = split(T, key);
        Treap tmp = new Treap(key, null, null);
        tmp = merge(pair.left, tmp);
        MainRoot = merge(tmp, pair.right);
    }

    public static Treap search(Treap T, long k) {
        if (T == null || k == T.key) {
            return T;
        }
        if (k < T.key) {
            return search(T.left, k);
        } else {
            return search(T.right, k);
        }
    }

    public static long SumOnSegment(Treap root, long leftBorder, long rightBorder) {
        Pair lq = split(root, leftBorder - 1);
        Pair rq = split(lq.right, rightBorder);
        //PrintTree(rq.left);
        long res = getSum(rq.left);
        MainRoot = merge(lq.left, merge(rq.left, rq.right));
        return res;
    }

    public static int findMaxDepth(Treap T, int depth) {
        if (T != null) {
            return Math.max(findMaxDepth(T.left, depth + 1), findMaxDepth(T.right, depth + 1));
        } else {
            return depth;
        }
    }

    public static void contLevelOrder(Treap T, int size) {
        Queue<Treap> queue = new LinkedList<>();
        int count = 0;
        do {
            MainQueue.add(T);
            if (T.left != null) {
                queue.add(T.left);
            } else {
                queue.add(new Treap(0, null, null));
            }
            if (T.right != null) {
                queue.add(T.right);
            } else {
                queue.add(new Treap(0, null, null));
            }
            if (!queue.isEmpty()) {
                T = queue.poll();
                count++;
            }
        } while (count < size);
    }

    public static void PrintTree(Treap Node) {

        int depth = findMaxDepth(Node, 0);
        System.out.println("Depth = " + depth);
        System.out.println();
        contLevelOrder(Node, (int) Math.pow(2, depth) - 1);

        int length = 0;
        Treap element;
        for (int i = 0; i < depth; i++) {
            for (int j = 0; j < 60 - length; j++) {
                System.out.print(" ");
            }
            for (int j = 0; j < Math.pow(2, i); j++) {
                element = MainQueue.poll();
                System.out.print("<" + element.key + "> " + "sum = " + element.sum);
                for (int k = 0; k < length - 2; k++) {
                    System.out.print(" ");
                }
            }
            System.out.println();
            length += 4;
        }
    }

    private static class FastReader {
        BufferedReader br;
        StringTokenizer st;

        FastReader(InputStream input) {
            br = new BufferedReader(new InputStreamReader(input));
        }

        String next() {
            while (st == null || !st.hasMoreElements()) {
                try {
                    st = new StringTokenizer(br.readLine());
                } catch (Exception e) {
                    return null;
                }
            }
            return st.nextToken();
        }

        long nextLong() {
            return Long.parseLong(next());
        }
    }
}

