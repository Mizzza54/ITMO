import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.StringTokenizer;

/**
 * @author Michale Gerasimov
 * start: 24.02.2020
 * @version -
 */
public class D {
    final static int MAX_N = 1 << 20;
    final static int MIDDLE = 500001;

    public static int[] count = new int[2 * MAX_N - 1];
    public static int[] left = new int[2 * MAX_N - 1];
    public static int[] right = new int[2 * MAX_N - 1];
    public static int[] sum = new int[2 * MAX_N - 1];
    public static int[] flag = new int[2 * MAX_N - 1];

    public static void push(int v, int len) {
        if (flag[v] != -1) {
            flag[2 * v + 1] = flag[v];
            flag[2 * v + 2] = flag[v];
            count[2 * v + 1] = flag[v];
            left[2 * v + 1] = flag[v];
            right[2 * v + 1] = flag[v];
            sum[2 * v + 1] = flag[v] * len;

            count[2 * v + 2] = flag[v];
            left[2 * v + 2] = flag[v];
            right[2 * v + 2] = flag[v];
            sum[2 * v + 2] = flag[v] * len;

            flag[v] = -1;
        }
    }

    public static void update(int v, int l, int r, int a, int b, int x) {
        if (b <= l || r <= a) {
            return;
        }
        if (l >= a && r <= b) {
            count[v] = x;
            left[v] = x;
            right[v] = x;
            sum[v] = x * (r - l);
            flag[v] = x;
            return;
        }
        push(v, (r - l) / 2);
        int m = (l + r) / 2;
        update(2 * v + 1, l, m, a, b, x);
        update(2 * v + 2, m, r, a, b, x);
        count[v] = count[2 * v + 1] + count[2 * v + 2];
        left[v] = left[2 * v + 1];
        right[v] = right[2 * v + 2];
        sum[v] = sum[2 * v + 1] + sum[2 * v + 2];
        if (right[2 * v + 1] == left[2 * v + 2] && left[2 * v + 2]  == 1) {
            count[v]--;
        }
    }

    public static void main(String[] args) {
        FastReader scanner = new FastReader(System.in);
        int n;
        n = scanner.nextInt();

        for (int i = 0; i < 2 * MAX_N - 1; i++) {
            count[i] = 0;
            left[i] = 0;
            right[i] = 0;
            sum[i] = 0;
            flag[i] = -1;
        }

        String line;
        int a, b, x;
        for (int i = 0; i < n; i++) {
            line = scanner.next();
            a = scanner.nextInt();
            b = scanner.nextInt();
            b = a + b;
            a += MIDDLE;
            b += MIDDLE;
            if (line.equals("B")) {
                x = 1;
            } else {
                x = 0;
            }
            update(0, MAX_N - 1, 2 * MAX_N - 1, a + MAX_N - 2, b + MAX_N - 2,  x);
            System.out.println(count[0] + " " + sum[0]);
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
