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
public class F {
    public static int MAX_N = 100001;
    public static int log_n = (int) (Math.log(MAX_N) / Math.log(2)) + 1;
    public static int[] a = new int[MAX_N];
    public static int[][] table = new int[MAX_N][log_n];
    public static int[] log = new int[MAX_N];
    public static int n, m, x;

    public static void main(String[] args) {
        FastReader scanner = new FastReader(System.in);
        n = scanner.nextInt();
        m = scanner.nextInt();
        x = scanner.nextInt();

        a[1] = x;
        for (int i = 2; i <= n; i++) {
            a[i] = (23 * a[i - 1] + 21563) % 16714589;
        }

        build();

        int u, v;
        u = scanner.nextInt();
        v = scanner.nextInt();
        int ans = result(u, v);
        for (int i = 2; i <= m; i++) {
            u = ((17 * u + 751 + ans + 2 * (i - 1)) % n) + 1;
            v = ((13 * v + 593 + ans + 5 * (i - 1)) % n) + 1;
            ans = result(u, v);
        }
        
        System.out.println(u + " " + v + " " + ans);
    }

    public static void build() {
        for (int j = 0; j < log_n; j++) {
            for (int i = (1 << j); i < MAX_N; i++) {
                log[i] = j;
            }
        }

        for (int i = 1; i <= n; i++) {
            table[i][0] = a[i];
        }

        for (int j = 1; (1 << j) <= n; j++) {
            for (int i = 1; (i + (1 << j) - 1) <= n; i++) {
                table[i][j] = Math.min(table[i][j - 1], table[i + (1 << (j - 1))][j - 1]);
            }
        }
    }

    public static int result(int a, int b) {
        int l = Math.min(a, b);
        int r = Math.max(a, b);
        int k = log[r - l + 1];
        return Math.min(table[l][k], table[r - (1 << k) + 1][k]);
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
