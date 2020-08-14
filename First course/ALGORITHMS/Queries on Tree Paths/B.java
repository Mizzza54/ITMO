import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.StringTokenizer;

/**
 * @author Michale Gerasimov
 * start: 15.05.2020
 * @version -
 */
public class B {
    public static FastReader scanner = new FastReader(System.in);
    public static int logn;
    public static int[] depth, p;
    public static int[][] dp;

    public static void main(String[] args) {
        int n, m;
        n = scanner.nextInt() + 1;
        logn = (int) (Math.log(n)/Math.log(2)) + 1;
        dp = new int[n][logn];
        depth = new int[n];
        p = new int[n];
        Arrays.fill(depth, 0);

        dp[0][0] = 0;
        p[0] = 0;
        depth[0] = 0;
        dp[1][0] = 0;
        p[1] = 0;
        depth[1] = 1;
        for (int i = 2, tmp; i < n; i++) {
            tmp = scanner.nextInt();
            dp[i][0] = tmp;
            p[i] = tmp;
            depth[i] = depth[tmp] + 1;
        }

        for (int j = 1; j < logn; j++) {
            for (int i = 0; i < n; i++){
                dp[i][j] = dp[dp[i][j - 1]][j - 1];
            }
        }

        m = scanner.nextInt();
        for (int i = 0, u, v; i < m; i++) {
            u = scanner.nextInt();
            v = scanner.nextInt();
            System.out.println(lca(u, v));
        }
    }

    public static int lca(int v, int u) {
        if (depth[v] > depth[u]) {
            int tmp = v;
            v = u;
            u = tmp;
        }

        for (int i = logn - 1; i > -1; i--) {
            if (depth[dp[u][i]] - depth[v] >= 0) {
                u = dp[u][i];
            }
        }

        if (v == u) {
            return v;
        }

        for (int i = logn - 1; i > -1; i--) {
            if (dp[v][i] != dp[u][i]) {
                v = dp[v][i];
                u = dp[u][i];
            }
        }
        return p[v];
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

        int nextInt() {
            return Integer.parseInt(next());
        }
    }
}
