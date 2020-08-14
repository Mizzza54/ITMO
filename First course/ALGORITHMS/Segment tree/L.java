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
public class L {
    final static int N = 1 << 7;
    public static int n;
    public static int[][][] t = new int[N + 1][N + 1][N + 1];

    public static int sum(int x, int y, int z) {
        int result = 0;
        for (int i = x; i >= 0; i = (i & (i + 1)) - 1) {
            for (int j = y; j >= 0; j = (j & (j + 1)) - 1) {
                for (int k = z; k >= 0; k = (k & (k + 1)) - 1) {
                    result += t[i][j][k];
                }
            }
        }
        return result;
    }

    public static void inc(int x, int y, int z, int add) {
        for (int i = x; i < n; i = i | (i + 1)) {
            for (int j = y; j < n; j = j | (j + 1)) {
                for (int k = z; k < n; k = k | (k + 1)) {
                    t[i][j][k] += add;
                }
            }
        }
    }

    public static void main(String[] args) {
        FastReader scanner = new FastReader(System.in);
        n = scanner.nextInt();
        for (int x = 0; x <= n; x++) {
            for (int y = 0; y <= n; y++) {
                for (int z = 0; z <= n; z++) {
                    t[x][y][z] = 0;
                }
            }
        }

        int x1, y1, z1, x2, y2, z2, k;
        String s;
        int m = 0;
        while (m != 3) {
            m = scanner.nextInt();
            if (m == 1) {
                x1 = scanner.nextInt();
                y1 = scanner.nextInt();
                z1 = scanner.nextInt();
                k = scanner.nextInt();
                inc(x1, y1, z1, k);
            }
            if (m == 2) {
                x1 = scanner.nextInt() - 1;
                y1 = scanner.nextInt() - 1;
                z1 = scanner.nextInt() - 1;
                x2 = scanner.nextInt();
                y2 = scanner.nextInt();
                z2 = scanner.nextInt();
                System.out.println(sum(x2, y2, z2) - sum(x2, y1, z2)  - sum(x1, y2, z2)
                        - sum(x2, y2, z1) + sum(x2, y1, z1) + sum(x1, y2, z1)
                        + sum(x1, y1, z2) - sum(x1, y1, z1) );
            }
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
