import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Random;
import java.util.StringTokenizer;

/**
 * @author Michale Gerasimov
 * start: 05.04.2020
 * @version -
 */
public class A {
    public static Random random = new Random();
    public static Treap MainRoot;

    public static void main(String[] args) {
        FastReader scanner = new FastReader(System.in);
        String line = scanner.next();
        int key;
        Treap tmp;
        boolean flag = true;
        while (line != null && !line.equals("quit")) {
            if (flag) {
                switch (line) {
                    case "insert":
                        key = scanner.nextInt();
                        MainRoot = new Treap(key,null, null);
                        flag = false;
                        break;
                    case "exists":
                        System.out.println("false");
                        break;
                    case "next":
                        System.out.println("none");
                        break;
                    case "prev":
                        System.out.println("none");
                        break;
                }
            } else {
                switch (line) {
                    case "insert":
                        key = scanner.nextInt();
                        MainRoot = insert(MainRoot, key);
                        break;
                    case "delete":
                        key = scanner.nextInt();
                        MainRoot = delete(MainRoot, key);
                        break;
                    case "exists":
                        key = scanner.nextInt();
                        if (exists(MainRoot, key)) {
                            System.out.println("true");
                        } else {
                            System.out.println("false");
                        }
                        break;
                    case "next":
                        key = scanner.nextInt();
                        tmp = next(key);
                        if (tmp == null) {
                            System.out.println("none");
                        } else {
                            System.out.println(tmp.key);
                        }
                        break;
                    case "prev":
                        key = scanner.nextInt();
                        tmp = prev(key);
                        if (tmp == null) {
                            System.out.println("none");
                        } else {
                            System.out.println(tmp.key);
                        }
                        break;
                }
            }
            line = scanner.next();
        }
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
        public int key;
        public int priority;
        public Treap left;
        public Treap right;

        Treap (int key, Treap left, Treap right) {
            this.key = key;
            this.priority = random.nextInt();
            this.left = left;
            this.right = right;
        }
    }

    public static Treap merge(Treap L, Treap R) {
        if (L == null) {
            return R;
        }
        if (R == null) {
            return L;
        }
        if (L.priority > R.priority) {
            L.right = merge(L.right, R);
            return L;
        } else {
            R.left = merge(L, R.left);
            return  R;
        }
    }

    public static Pair split(Treap T, int key) {
        if (T == null) {
            return new Pair(null, null);
        } else if (key >= T.key) {
            Pair pair = split(T.right, key);
            T.right = pair.left;
            return new Pair(T, pair.right);
        } else {
            Pair pair = split(T.left, key);
            T.left = pair.right;
            return new Pair(pair.left, T);
        }
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

    public static boolean exists(Treap T, int x) {
        if (search(T, x) == null) {
            return false;
        } else {
            return true;
        }
    }

    public static Treap maximum(Treap T) {
        if (T.right == null) {
            return T;
        }
        return maximum(T.right);
    }

    public static Treap minimum(Treap T) {
        if (T.left == null) {
            return T;
        }
        return minimum(T.left);
    }

    public static Treap next(int T) {
        Treap current = MainRoot;
        Treap successor = null;
        while (current != null) {
            if (current.key > T) {
                successor = current;
                current = current.left;
            } else {
                current = current.right;
            }
        }
        return successor;
    }

    public static Treap prev(int T) {
        Treap current = MainRoot;
        Treap successor = null;
        while (current != null) {
            if (current.key < T) {
                successor = current;
                current = current.right;
            } else {
                current = current.left;
            }
        }
        return successor;
    }

    public static void inorderTraversal (Treap A) {
        if (A != null) {
            inorderTraversal(A.left);
            System.out.println(A.key);
            inorderTraversal(A.right);
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

