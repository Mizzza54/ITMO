import java.io.*;
import java.util.Arrays;
import java.util.StringTokenizer;

/**
 * @author Michale Gerasimov
 * start: 15.05.2020
 * @version -
 */
public class C {
    public static FastReader scanner;

    static {
        try {
            scanner = new FastReader("minonpath.in");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("minonpath.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static int logn;
    public static int[] depth;
    public static int[][] dp, minPrice;

    public static void main(String[] args) {
        int n, m;
        n = scanner.nextInt() + 1;
        logn = (int) (Math.log(n)/Math.log(2)) + 1;
        dp = new int[n][logn];
        minPrice = new int[n][logn];
        depth = new int[n];
        Arrays.fill(depth, 0);

        minPrice[0][0] = 1000000;
        dp[0][0] = 0;
        depth[0] = 0;
        minPrice[1][0] = 1000000;
        dp[1][0] = 0;
        depth[1] = 1;

        for (int i = 2, tmp1, tmp2; i < n; i++) {
            tmp1 = scanner.nextInt();
            tmp2 = scanner.nextInt();
            dp[i][0] = tmp1;
            depth[i] = depth[tmp1] + 1;
            minPrice[i][0] = tmp2;
        }

        for (int j = 1; j < logn; j++) {
            for (int i = 0; i < n; i++){
                dp[i][j] = dp[dp[i][j - 1]][j - 1];
                minPrice[i][j] = Math.min(minPrice[i][j - 1], minPrice[dp[i][j - 1]][j - 1]);
            }
        }

        m = scanner.nextInt();
        for (int i = 0, u, v; i < m; i++) {
            u = scanner.nextInt();
            v = scanner.nextInt();
            writer.println(lca(u, v));
        }

        writer.close();
    }

    public static int lca(int v, int u) {
        int answer = 1000000;

        if (depth[v] > depth[u]) {
            int tmp = v;
            v = u;
            u = tmp;
        }

        for (int i = logn - 1; i > -1; i--) {
            if (depth[dp[u][i]] - depth[v] >= 0) {
                answer = Math.min(answer, minPrice[u][i]);
                u = dp[u][i];
            }
        }

        if (v == u) {
            return answer;
        }

        for (int i = logn - 1; i > -1; i--) {
            if (dp[v][i] != dp[u][i]) {
                answer = Math.min(answer, Math.min(minPrice[v][i], minPrice[u][i]));
                v = dp[v][i];
                u = dp[u][i];
            }
        }
        return Math.min(Math.min(minPrice[v][0], minPrice[u][0]), answer);
    }

    private static class FastReader {
        BufferedReader br;
        StringTokenizer st;

        FastReader(String input) throws FileNotFoundException {
            br = new BufferedReader(new FileReader(input));
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
