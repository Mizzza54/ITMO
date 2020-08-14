import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.StringTokenizer;

/**
 * @author Michale Gerasimov
 * start: 19.12.2019
 * @version -
 */

public class Aquarium {
    private static FastReader reader = new FastReader();
    private static int n;
    private static long cost;
    private static long[][] dp;
    private static long[][] distance;
    private static int[][] prev;
    private static ArrayList<Integer> path = new ArrayList<>();

    public static void main(String[] args) {
        n = reader.nextInt();
        distance = new long[n][n];
        dp = new long[n][(int) Math.pow(2, n)];
        prev = new int[n][(int) Math.pow(2, n)];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                distance[i][j] = reader.nextInt();
            }
        }
        findWay(start());
        System.out.println(cost);
        Collections.reverse(path);
        for (Integer integer : path) {
            System.out.print(integer + " ");
        }
    }

    private static int start() {
        for (int i = 0; i < n; i++) {
            for (int mask = 0; mask < (int) Math.pow(2, n); mask++) {
                dp[i][mask] = Integer.MAX_VALUE;
            }
        }

        for (int i = 0; i < n; i++) {
            dp[i][0] = 0;
        }

        for (int mask = 0; mask < (int) Math.pow(2, n); mask++) {
            for (int i = 0; i < n; i++) {
                if (getBit(mask, i) == 1) {
                    for (int j = 0; j < n; j++) {
                        if (dp[j][mask - (int) Math.pow(2, i)] + distance[i][j] < dp[i][mask]) {
                            dp[i][mask] = dp[j][mask - (1 << i)] + distance[i][j];
                            prev[i][mask] = j;
                        }
                    }
                }
            }
        }

        cost = 100000000;
        int last = 0;
        for (int i = 0; i < n; i++) {
            if (dp[i][(int) Math.pow(2, n) - 1] < cost) {
                cost = dp[i][(1 << n) - 1];
                last = i;
            }
        }
        return last;
    }

    private static int getBit(int x, int i) {
        return ((x >> i) & 1);
    }

    private static void findWay(int last) {
        int tmp;
        int mask = (int) Math.pow(2, n) - 1;
        for (int i = 0; i < n; i++) {
            path.add(last + 1);
            tmp = last;
            last = prev[last][mask];
            mask -= (int) Math.pow(2, tmp);
        }
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