
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Random;
import java.util.StringTokenizer;

/**
 * @author Michale Gerasimov
 * start: 17.04.2020
 * @version -
 */
public class G {
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
            MainRoot = move(MainRoot, left, right);
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
        public long key, priority;
        public long size = 1;
        public Treap left;
        public Treap right;

        Treap (long key, Treap left, Treap right) {
            this.key = key;
            this.priority = random.nextInt();
            this.left = left;
            this.right = right;
        }
    }

    public static void updateSize(Treap T) {
        if (T == null) {
            return;
        }
        T.size = 1 + getSize(T.left) + getSize(T.right);
    }

    public static Treap merge(Treap L, Treap R) {
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
        Treap tmp = new Treap(key, null, null);
        tmp = merge(pair.left, tmp);
        return merge(tmp, pair.right);
    }

    public static Treap remove(Treap T, long pos) {
        Pair pair1 = split(T, pos);
        Pair pair2 = split(pair1.left, pos - 1);
        return merge(pair2.left, pair1.right);
    }

    public static Treap move(Treap T, long left, long right) {
        Pair lp = split(T, left - 1);
        Pair rp = split (lp.right, right - left + 1);
        return merge(rp.left, merge(lp.left, rp.right));
    }

    public static void inorderTraversal (Treap T) {
        if (T != null) {
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
