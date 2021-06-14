import java.io.*;

/**
 * @author Michael Gerasimov
 * start: 12.04.2021
 * @version -
 */
public class F {
    StreamTokenizer scanner = new StreamTokenizer(new BufferedReader(new InputStreamReader(System.in)));
    PrintWriter writer = new PrintWriter(System.out);
    long MOD = 1_000_000_000 + 7;

    public static void main(String[] args) {
        F F = new F();
        F.run();
    }

    public void run() {
        int k, m;
        k = nextInt();
        m = nextInt();
        int[] weight = new int[k];
        long[] sum = new long[m + 1];
        long[] answer = new long[m + 1];
        for (int i = 0; i < k; i++) {
            weight[i] = nextInt();
        }


        answer[0] = 1;
        sum[0] = 1;
        for (int i = 1; i <= m; i++) {
            for (int j = 0; j < k; j++) {
                if (i >= weight[j]) {
                    answer[i] = (answer[i] + sum[i - weight[j]]) % MOD;
                }
            }
            for (int j = 0; j <= i; j++) {
                sum[i] = (sum[i] + answer[j] * answer[i - j]) % MOD;
            }
        }

        for (int i = 1; i <= m; i++) {
            writer.write(Long.toString(answer[i]));
            writer.write(" ");
        }

        writer.flush();
        writer.close();
    }

    int nextInt() {
        try {
            scanner.nextToken();
            return (int) scanner.nval;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return -42;
    }
}