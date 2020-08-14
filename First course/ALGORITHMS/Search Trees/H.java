
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 17.04.2020
 * @version -
 */
public class H {
    public static FastReader scanner = new FastReader(System.in);
    public static Random random = new Random();
    public static Treap MainRoot;

    public static void main(String[] args) {
        long n = scanner.nextLong();
        long m = scanner.nextLong();

        for (long i = 0; i < n; i++) {
            MainRoot = insert(MainRoot, i + 1, i + 1);
        }

        long left, right;
        for (long i = 0; i < m; i++) {
            left = scanner.nextLong();
            right = scanner.nextLong();
            MainRoot = reverse(MainRoot, left, right);
        }
        inorderTraversal(MainRoot);
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
        public long key, priority, size, position;
        public Treap left;
        public Treap right;
        boolean reversed;

        Treap (long key, long position, Treap left, Treap right) {
            this.key = key;
            this.priority = random.nextLong();
            this.left = left;
            this.right = right;
            this.reversed = false;
            this.size = 1;
            this.position = position;
        }
    }

    public static void updateSize(Treap T) {
        if (T == null) {
            return;
        }
        T.size = 1 + getSize(T.left) + getSize(T.right);
    }

    public static void push(Treap T) {
        if (T == null) {
            return;
        }

        if (T.reversed) {
            Treap tmp = T.left;
            T.left = T.right;
            T.right = tmp;
            T.reversed = false;
            if (T.left != null) {
                T.left.reversed ^= true;
            }
            if (T.right != null) {
                T.right.reversed ^= true;
            }
        }
    }

    public static Treap merge(Treap L, Treap R) {
        push(L);
        push(R);

        if (L == null) {
            updateSize(R);
            return R;
        }
        if (R == null) {
            updateSize(L);
            return L;
        }
        if (L.priority > R.priority) {
            L.right = merge(L.right, R);
            updateSize(L);
            return L;
        } else {
            R.left = merge(L, R.left);
            updateSize(R);
            return  R;
        }
    }

    public static long getSize(Treap T) {
        if (T == null) {
            return  0;
        } else {
            return T.size;
        }
    }

    public static Pair split(Treap T, long key) {
        if (T == null) {
            return new Pair(null, null);
        }

        push(T);
        if (getSize(T.left) >= key) {
            Pair pair = split(T.left, key);
            T.left = pair.right;
            updateSize(T);
            return new Pair(pair.left, T);
        } else {
            Pair pair = split(T.right, key - getSize(T.left) - 1);
            T.right = pair.left;
            updateSize(T);
            return new Pair(T, pair.right);
        }
    }

    public static Treap insert(Treap T, long key, long pos) {
        Pair pair = split(T, pos);
        Treap tmp = new Treap(key, pos, null, null);
        tmp = merge(pair.left, tmp);
        return merge(tmp, pair.right);
    }

    public static Treap reverse(Treap T, long left, long right) {
        Pair pair1 = split(T, left - 1);
        Pair pair2 = split(pair1.right ,right - left + 1);
        pair2.left.reversed ^= true;
        Treap tmp = merge(merge(pair1.left, pair2.left), pair2.right);
        return tmp;
    }

    public static void inorderTraversal (Treap T) {
        if (T != null) {
            push(T);
            inorderTraversal(T.left);
            System.out.print(T.key + " ");
            inorderTraversal(T.right);
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
