import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.StringTokenizer;

/**
 * @author Michale Gerasimov
 * start: 24.02.2020
 * @version -
 */
public class H {

    public static class Query {
        public int l, r;
        public long res;

        Query(int l, int r, long res) {
            this.l = l;
            this.r = r;
            this.res = res;
        }
    }

    public static class MyComparator implements Comparator<Query> {
        public int compare(Query a, Query b) {
            return Long.compare(a.res, b.res);
        }
    }

    public static ArrayList<Query> arr = new ArrayList<>();
    public static long[] tree;
    public static int sizeTree;

    public static void main(String[] args) throws FileNotFoundException {
        FastReader scanner = new FastReader("rmq.in");
        PrintWriter writer = new PrintWriter("rmq.out");
        Comparator cmp = new MyComparator();

        int n, m;
        n = scanner.nextInt();
        m = scanner.nextInt();

        sizeTree = 1;
        while (sizeTree < n) {
            sizeTree *= 2;
        }
        tree = new long[2 * sizeTree - 1];
        for (int i = 0; i < 2 * sizeTree - 1; i++) {
            tree[i] = Long.MAX_VALUE;
        }

        int x1, x2;
        long val;
        for (int i = 0; i < m; i++) {
            x1 = scanner.nextInt();
            x2 = scanner.nextInt();
            val = scanner.nextInt();
            arr.add(new Query(x1,x2,val));
        }
        Collections.sort(arr, cmp);

        for (int i = 0; i < m; i++) {
            update(0, 0, sizeTree, arr.get(i).l - 1, arr.get(i).r, arr.get(i).res);
        }

        build(0);

        for (int i = sizeTree - 2; i >= 0; i--) {
            tree[i] = Math.min(tree[2 * i + 1], tree[2 * i + 2]);
        }

        boolean flag = true;
        for (int i = 0; i < m; i++) {
            val = result(0, 0, sizeTree, arr.get(i).l - 1, arr.get(i).r);
            if (val != arr.get(i).res) {
                flag = false;
                break;
            }
        }

        if (flag) {
            writer.println("consistent");
            for (int i = sizeTree - 1; i < sizeTree + n - 1; i++) {
                if (tree[i] == Long.MAX_VALUE) {
                    writer.print(Integer.MAX_VALUE + " ");
                }
                else {
                    writer.print(tree[i] + " ");
                }
            }
        } else {
            writer.println("inconsistent");
        }
        writer.close();
    }

    public static void update(int v, int l, int r, int L, int R, long x) {
        //push
        if (R <= l || r <= L) {
            return;
        }
        if (l >= L && r <= R) {
            tree[v] = x;
            //push
            return;
        }
        int m = (l + r) / 2;
        update(2 * v + 1, l, m, L, R, x);
        update(2 * v + 2, m, r, L, R, x);
        //tree[v] = Math.min(tree[2 * v + 1], tree[2 * v + 2]);
    }

    public static long result(int v, int l, int r, int L, int R) {
        //push
        if (R <= l || r <= L) {
            return Long.MAX_VALUE;
        }
        if (l >= L && r <= R) {
            return tree[v];
        }
        //push
        int m = (l + r) / 2;
        long minl = result(2 * v + 1, l, m, L, R);
        long minr = result(2 * v + 2, m, r, L, R);
        return Math.min(minl, minr);
    }

    public static void check(int v) {
        if (tree[v] == Long.MAX_VALUE) {
            tree[v] = tree[(v - 1) / 2];
        }
        if (tree[v] < tree[(v - 1) / 2]) {
            tree[v] = tree[(v - 1) / 2];
        }
    }

    public static void build(int v) {
        if (v >= sizeTree - 1) {
            return;
        }
        if (tree[v] != Long.MAX_VALUE) {
            check(2 * v + 1);
            check(2 * v + 2);
        }
        build(2 * v + 1);
        build(2 * v + 2);
    }

    static class FastReader {
        BufferedReader br;
        StringTokenizer st;

        FastReader(String name) throws FileNotFoundException {
            br = new BufferedReader(new
                    InputStreamReader(new
                    FileInputStream(name)));
        }

        String next() {
            while (st == null || !st.hasMoreElements()) {
                try {
                    st = new StringTokenizer(br.readLine());
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
            return st.nextToken();
        }

        int nextInt() {
            return Integer.parseInt(next());
        }
    }
}
