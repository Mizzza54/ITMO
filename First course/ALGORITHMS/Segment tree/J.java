import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.StringTokenizer;

/**
 * @author Michale Gerasimov
 * start: 24.02.2020
 * @version -
 */
public class J {
    public static FastReader scanner = new FastReader(System.in);
    public static int sizeTree, n, m;
    public static Node[] t;
    public static long[] set;

    public static class Node {
        public long min;
        public int index;
        Node(long min, int index) {
            this.min = min;
            this.index = index;
        }
    }

    public static void main(String[] args) {
        n = scanner.nextInt();
        m = scanner.nextInt();
        build();

        String str;
        int a, b, c;
        for (int i = 0; i < m; i++) {
            str = scanner.next();
            a = scanner.nextInt() - 1;
            b = scanner.nextInt();
            if (str.equals("defend")) {
                c = scanner.nextInt();
                update(0, 0, sizeTree, a, b, c);
            }
            if (str.equals("attack")) {
                Node res = result(0, 0, sizeTree, a, b);
                System.out.println(res.min + " " + res.index);
            }
        }
    }

    public static void push(int v, int l, int r) {
        if (set[v] != Long.MAX_VALUE) {
            if (t[v].min < set[v]) {
                t[v] = new Node(set[v], t[v].index);
                if (r - l != 1) {
                    set[2 * v + 1] = set[v];
                    set[2 * v + 2] = set[v];
                }
            }
            set[v] = Long.MAX_VALUE;
        }
    }

    public static void update(int v, int l, int r, int a, int b, int x) {
        push(v, l, r);
        if (b <= l || r <= a) {
            return;
        }
        if (l >= a && r <= b) {
            set[v] = x;
            push(v, l, r);
            return;
        }
        int m = (l + r) / 2;
        update(2 * v + 1, l, m, a, b, x);
        update(2 * v + 2, m, r, a, b, x);
        if (t[2 * v + 1].min <= t[2 * v + 2].min) {
            t[v] = t[2 * v + 1];
        } else {
            t[v] = t[2 * v + 2];
        }
    }

    public static Node result(int v, int l, int r, int a, int b) {
        push(v, l, r);
        if (b <= l || r <= a) {
            return new Node(Long.MAX_VALUE, -1);
        }
        if (l >= a && r <= b) {
            return t[v];
        }
        int m = (l + r) / 2;
        push(v, l, r);
        Node lres = result(2 * v + 1, l, m, a, b);
        Node rres = result(2 * v + 2, m, r, a, b);
        if (lres.min <= rres.min) {
            return lres;
        } else {
            return rres;
        }
    }

    public static void build() {
        sizeTree = 1;
        while (sizeTree < n) {
            sizeTree *= 2;
        }

        t = new Node[sizeTree * 2 - 1];
        set = new long[sizeTree * 2 - 1];
        Arrays.fill(set, Long.MAX_VALUE);

        for (int i = sizeTree - 1; i < sizeTree + n - 1; i++) {
            t[i] = new Node(0, i - sizeTree + 2);
        }
        for (int i = sizeTree + n - 1; i < t.length; i++) {
            t[i] = new Node(Long.MAX_VALUE, i - sizeTree + 2);
        }
        for (int i = sizeTree - 2; i >= 0; i--) {
            t[i] = new Node(Math.min(t[2 * i + 1].min, t[2 * i + 2].min), t[2 * i + 1].index);
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
