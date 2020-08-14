import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.StringTokenizer;

/**
 * @author Michale Gerasimov
 * start: 20.12.2019
 * @version -
 */
public class CutePatterns {
    private static FastReader reader = new FastReader();
    private static int n, m;

    public static void main(String[] args) {
        n = reader.nextInt();
        m = reader.nextInt();
        if (n > m) {
            int tmp = n;
            n = m;
            m = tmp;
        }
        long[][] dp = new long[1 << n][1 << n];
        long[][] a = new long[m][1 << n];
        for (int i = 0; i < (1 << n); i++) {
            for (int j = 0; j < (1 << n); j++) {
                if (checkProfile(i, j)) {
                    dp[i][j] = 1;
                } else {
                    dp[i][j] = 0;
                }
            }
        }
        for (int i = 0; i < (1 << n); i++) {
            a[0][i] = 1;
        }
        for (int k = 1; k < m; k++) {
            for (int i = 0; i < (1 << n); i++) {
                for (int j = 0; j < (1 << n); j++) {
                    a[k][i] = a[k][i] + a[k - 1][j] * dp[j][i];
                }
            }
        }
        long ans = 0;
        for (int i = 0; i < (1 << n); i++) {
            ans = ans + a[m - 1][i];
        }
        System.out.println(ans);
    }

    public static boolean checkProfile(int x, int y) {
        for (int j = 0; j < n - 1; j++)
            if ((x >> j & 1) == (y >> j & 1) && (y >> j & 1) == (x >> (j + 1) & 1)
                    && (x >> (j + 1) & 1) == (y >> (j + 1) & 1)) {
                return false;
            }
        return true;
    }

    private static class FastReader {
        BufferedReader br;
        StringTokenizer st;

        FastReader() {
            br = new BufferedReader(new InputStreamReader(System.in));
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
    }
}
