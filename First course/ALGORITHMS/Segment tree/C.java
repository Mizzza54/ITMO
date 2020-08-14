import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.StringTokenizer;

/**
 * @author Michale Gerasimov
 * start: 23.02.2020
 * @version -
 */
public class C {
    public static FastReader scanner = new FastReader(System.in);
    public static long[] t;
    public static long[] upd;
    public static boolean[] set;
    public static int id;
    public static void main(String[] args) {
        int n = scanner.nextInt();
        int sizeTree = (int) (Math.log(n) / Math.log(2));
        sizeTree++;
        long[] data = new long[(int) Math.pow(2, sizeTree)];
        for (int i = 0; i < n; i++) {
            data[i] = scanner.nextLong();
        }
        for (int i = n; i < data.length; i++) {
            data[i] = Long.MAX_VALUE;
        }
        sizeTree = (int) (Math.pow(2, sizeTree + 1));
        id = sizeTree / 2 - 1;
        sizeTree--;
        t = new long[sizeTree];
        upd = new long[sizeTree];
        set = new boolean[sizeTree];
        build(data, 0, 0, data.length);

        //printTree();


        String line = "qwerty";
        line = scanner.nextLine();
        while (line != null) {
            int i = 4;
            while (Character.isDigit(line.charAt(i))) {
                i++;
            }
            int a = Integer.parseInt(line.substring(4, i)) - 1;
            i++;
            int k = i;
            while (Character.isDigit(line.charAt(k))) {
                k++;
                if (k == line.length()) {
                    break;
                }
            }
            int b = Integer.parseInt(line.substring(i, k));
            k++;
            long x;
            switch (line.charAt(1)) {
                case 'e': // set
                    x = Long.parseLong(line.substring(k, line.length()));
                    set(0,0, data.length, a, b, x);
                    //printTree();
                    break;
                case 'd': // add
                    x = Integer.parseInt(line.substring(k, line.length()));
                    update(0,0, data.length, a, b, x);
                    //printTree();
                    break;
                case 'i': // min
                    System.out.println(rmq(0, 0, data.length, a, b));
                    break;
            }
            line = scanner.nextLine();
        }
    }

    public static void build (long[] a, int v, int l, int r) {
        if (r - l == 1) {
            t[v] = a[l];
        } else {
            int m = (l + r) / 2;
            build(a, 2 * v + 1, l, m);
            build(a, 2 * v + 2, m, r);
            t[v] = Math.min(t[2 * v + 1], t[2 * v + 2]);
        }
    }

    public static void push(int v, int l, int r) {
        if (r - l == 1) {
            return;
        }
        if (set[v]) {
            t[2 * v + 1] = t[v];
            upd[2 * v + 1] = 0;
            set[2 * v + 1] = true;
            t[2 * v + 2] = t[v];
            upd[2 * v + 2] = 0;
            set[2 * v + 2] = true;
            set[v] = false;
            return;
        }
        t[2 * v + 1] += upd[v];
        t[2 * v + 2] += upd[v];
        if (!set[2 * v + 1]) {
            upd[2 * v + 1] += upd[v];
        }
        if (!set[2 * v + 2]) {
            upd[2 * v + 2] += upd[v];
        }
        upd[v] = 0;
        return;
    }

    public static void update(int v, int l, int r, int a, int b, long x) {
        if (b <= l || a >= r) {
            return;
        }
        if (l >= a && r <= b) {
            t[v] += x;
            if (!set[v])
                upd[v] += x;
            push(v, l, r);
            return;
        }
        push(v, l, r);
        int m = (l + r) / 2;
        update(2 * v + 1, l, m, a, b, x);
        update(2 * v + 2, m, r, a, b, x);
        t[v] = Math.min(t[2 * v + 1], t[2 * v + 2]);
    }

    public static long rmq(int v, int l, int r, int a, int b) {
        if (l >= a && r <= b) {
            return t[v];
        }
        if (l >= b || r <= a) {
            return Long.MAX_VALUE;
        }
        push(v, l, r);
        int m = (l + r) / 2;
        return Math.min(rmq(2 * v + 1, l, m, a, b), rmq(2 * v + 2, m, r, a, b));
    }

    public static void set(int v, int l, int r, int a, int b, long x) {
        if (b <= l || a >= r) {
            return;
        }
        if (l >= a && r <= b) {
            t[v] = x;
            upd[v] = 0;
            set[v] = true;
            return;
        }
        push(v, l, r);
        int m = (l + r) / 2;
        set(2 * v + 1, l, m, a, b, x);
        set(2 * v + 2, m, r, a, b, x);
        t[v] = Math.min(t[2 * v + 1], t[2 * v + 2]);
    }

    public static void printTree() {
        int q = 2;
        for (int i = 0; i < t.length; i++) {
            if (i + 1 == q) {
                q *= 2;
                System.out.println();
            }
            System.out.print(t[i] + upd[i] + " ");
        }
        System.out.println();
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
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
            return st.nextToken();
        }

        int nextInt() {
            return Integer.parseInt(next());
        }

        long nextLong() {
            return Long.parseLong(next());
        }

        boolean hasNext() {
            if (st.hasMoreElements()) {
                return true;
            } else {
                return false;
            }
        }

        public String nextLine() {
            if (st == null || !st.hasMoreTokens()) {
                try {
                    return br.readLine();
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }

            return st.nextToken();
        }
    }
}
