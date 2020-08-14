
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 05.04.2020
 * @version -
 */
public class E {
    public static Random random = new Random();
    public static Treap MainRoot;
    public static Queue<Treap> MainQueue = new LinkedList<>();

    public static void main(String[] args) {
        FastReader scanner = new FastReader(System.in);
        int n = scanner.nextInt();
        String line;
        boolean flag = true;

        for (int i = 0; i < n; i++) {
            line = scanner.next();
            if (flag) {
                switch (line) {
                    case "+1":
                    case "1":
                        MainRoot = new Treap(scanner.nextInt(), null, null);
                        flag = false;
                        break;
                }
            } else {
                switch (line) {
                    case "+1":
                    case "1":
                        MainRoot = insert(MainRoot, scanner.nextInt());
                        break;
                    case "0":
                        int in = scanner.nextInt();
                        //System.out.println("in = " + in);
                        System.out.println(kthElement(MainRoot, getSize(MainRoot) - in + 1));
                        break;
                    case "-1":
                        MainRoot = delete(MainRoot, scanner.nextInt());
                        break;
                }
            }
        }
        /*
        int depth = findMaxDepth(MainRoot, 0);
        System.out.println("Depth = " + depth);
        System.out.println();
        contLevelOrder(MainRoot, (int) Math.pow(2, depth) - 1);
        PrintTree(depth);
         */
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
        public int key, priority, sum, size;
        public Treap left, right;

        Treap (int key, Treap left, Treap right) {
            this.key = key;
            this.priority = random.nextInt();
            this.left = left;
            this.right = right;
            this.sum = 0;
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

    public static Pair split(Treap T, int key) {
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

    public static int getSum(Treap T) {
        if (T == null) {
            return  0;
        } else {
            return T.sum;
        }
    }

    public static int getSize(Treap T) {
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

    public static Treap insert(Treap T, int key) {
        Pair pair = split(T, key);
        Treap tmp = new Treap(key, null, null);
        tmp = merge(pair.left, tmp);
        return merge(tmp, pair.right);
    }

    public static Treap delete(Treap T, int key) {
        Pair pair1 = split(T, key);
        Pair pair2 = split(pair1.left, key - 1);
        return merge(pair2.left, pair1.right);
    }

    public static Treap search(Treap T, int k) {
        if (T == null || k == T.key) {
            return T;
        }
        if (k < T.key) {
            return search(T.left, k);
        } else {
            return search(T.right, k);
        }
    }

    public static int kthElement(Treap T, int n) {
        int number = getSize(T.left);
        if (number + 1 == n) {
            return T.key;
        }
        if (n <= number) {
            return kthElement(T.left, n);
        }
        else {
            return kthElement(T.right, n - number - 1);
        }
    }
    
    public static void inorderTraversal (Treap A) {
        if (A != null) {
            inorderTraversal(A.left);
            System.out.println(A.key);
            inorderTraversal(A.right);
        }
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

    public static void PrintTree(int depth) {
        int length = 0;
        Treap element;
        for (int i = 0; i < depth; i++) {
            for (int j = 0; j < 60 - length; j++) {
                System.out.print(" ");
            }
            for (int j = 0; j < Math.pow(2, i); j++) {
                element = MainQueue.poll();
                System.out.print("<" + element.key + "> " + "size = " + element.size);
                for (int k = 0; k < length; k++) {
                    System.out.print(" ");
                }
            }
            System.out.println();
            length += 2;
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

        int nextInt() {
            return Integer.parseInt(next());
        }
    }
}

