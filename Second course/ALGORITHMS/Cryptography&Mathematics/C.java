import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 25.05.2021
 * @version -
 */
public class C {
    PrintWriter writer = new PrintWriter(System.out);
    FastReader scanner = new FastReader(System.in);
    private static int INF = 1000000000;

    public static void main(String[] args) {
        C C = new C();
        C.run();
    }

    public void run() {
        long a, b, n, m, temp;
        a = scanner.nextLong();
        b = scanner.nextLong();
        n = scanner.nextLong();
        m = scanner.nextLong();
        if (n < m) {
            temp = a;
            a = b;
            b = temp;
            temp = n;
            n = m;
            m = temp;
        }

        for (long i = 0; i < n; i++) {
            long res =  i * n + a;
            if (res % m == b) {
                writer.println(res);
                writer.flush();
                writer.close();
                return;
            }
        }
        writer.flush();
        writer.close();
    }
}

class FastReader {
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

    long nextLong() {
        return Long.parseLong(next());
    }
}
