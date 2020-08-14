import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Random;
import java.util.StringTokenizer;

/**
 * @author Michale Gerasimov
 * start: 21.04.2020
 * @version -
 */
public class C {
    public static FastReader scanner = new FastReader(System.in);
    public static Random random = new Random();
    public static Treap MainRoot;
    public static ArrayList<Long> Result = new ArrayList<>();
    public static long m;

    public static void main(String[] args) {
        long n = scanner.nextLong();
        m = scanner.nextLong();


        for (long i = 0; i < m + n; i++) {
            MainRoot = insertBegin(MainRoot, 0, i);
        }

        for (long i = 0; i < n; i++) {
            MainRoot = insert(MainRoot, i + 1, scanner.nextLong() - 1);
        }
        //MainRoot = remove(MainRoot, 1);
        //System.out.println(MainRoot.size);
        inorderTraversal(MainRoot);
        int tmp = Result.size();
        for (int i = tmp - 1; i > -1; i--) {
            if (Result.get(i) == 0) {
                Result.remove(i);
            } else {
                break;
            }
        }
        System.out.println(Result.size());
        for (int i = 0; i < Result.size(); i++) {
            System.out.print(Result.get(i) + " ");
        }
        //System.out.println(getNode(MainRoot, 1).key);
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
        public long zero;

        Treap (long key, Treap left, Treap right) {
            this.key = key;
            this.priority = random.nextInt();
            this.left = left;
            this.right = right;
            if (key == 0) {
                this.zero = 1;
            } else {
                this.zero = 0;
            }
        }
    }

    public static void updateSize(Treap T) {
        if (T == null) {
            return;
        }
        T.size = 1 + getSize(T.left) + getSize(T.right);
        T.zero = getZero(T.left) + getZero(T.right) + (T.key != 0 ? 0 : 1);
    }

    public static long getZero(Treap T)
    {
        if (T == null) {
            return  0;
        } else {
            return T.zero;
        }
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

    public static Treap getNode(Treap T, long pos) {
        if (T == null) {
            return null;
        }
        long index = getSize(T.left);
        if (pos < index) {
            return getNode(T.left, pos);
        }
        else if (pos == index) {
            return T;
        }
        else {
            return getNode(T.right, pos - index - 1);
        }
    }

    public static Treap removeZero(Treap T)
    {
        if (T.left != null && T.left.zero > 0) {
            T.left = removeZero(T.left);
        }
        else if (T.key == 0) {
            T = merge(T.left, T.right);
        }
        else {
            T.right = removeZero(T.right);
        }
        updateSize(T);
        return T;
    }

    public static Treap insert(Treap T, long key, long pos) {
        Pair pair = split(T, pos);
        Treap tmp = new Treap(key, null, null);
        tmp = merge(pair.left, tmp);
        pair.right = removeZero(pair.right);
        return merge(tmp, pair.right);
    }

    public static Treap insertBegin(Treap T, long key, long pos) {
        T = merge(T, new Treap(key, null, null));
        return T;
    }

    public static Treap remove(Treap T, long pos) {
        Pair pair1 = split(T, pos);
        Pair pair2 = split(pair1.left, pos - 1);
        return merge(pair2.left, pair1.right);
    }

    public static void inorderTraversal (Treap T) {
        if (T != null) {
            inorderTraversal(T.left);
            //System.out.print(T.key + " ");
            Result.add(T.key);
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
