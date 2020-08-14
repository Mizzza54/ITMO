import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.StringTokenizer;

/**
 * @author Michale Gerasimov
 * start: 17.02.2020
 * @version -
 */
public class A {

    public static void main(String[] args) {
        FastReader scanner = new FastReader(System.in);

        int n, x, y;
        n = scanner.nextInt();
        x = scanner.nextInt();
        y = scanner.nextInt();
        long[] a = new long[10000000];
        // 3 5 7
        // 3 8 15
        a[0] = scanner.nextLong();
        for (int i = 1; i < n; i++) {
            a[i] = (x * a[i - 1] + y) & ((1 << 16) - 1);
        }
        for (int i = 1; i < n; i++) {
            a[i] = a[i - 1] + a[i];
        }

        int m, z, t;
        m = scanner.nextInt();
        z = scanner.nextInt();
        t = scanner.nextInt();
        int[] b = new int[2 * 10000000];
        b[0] = scanner.nextInt();

        for (int i = 1; i < 2 * m; i++) {
            b[i] = (z * b[i - 1] + t) & ((1 << 30) - 1);
        }

        for (int i = 0; i < 2 * m; i++)
        {
            b[i] = b[i] % n;
            if (b[i] < 0) {
                b[i] = 0;
            }
        }


        long result = 0;
        for (int i = 0; i < m; i++) {
            int left = Math.min(b[2 * i], b[2 * i + 1]);
            int right = Math.max(b[2 * i], b[2 * i + 1]);
            if (left > 0) {
                result += a[right] - a[left - 1];
            } else {
                result += a[right];
            }
        }

        System.out.println(result);
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
