import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.StringTokenizer;

/**
 * @author Michael Gerasimov
 */
public class K {
    public static void main(String[] args) {
        FastReader scanner = new FastReader(System.in);
        int n = scanner.nextInt();

        double[][] objects = new double[n][2];
        double average1 = 0;
        double average2 = 0;
        for (int i = 0; i < n; i++) {
            objects[i][0] = scanner.nextInt();
            objects[i][1] = scanner.nextInt();

            average1 += objects[i][0];
            average2 += objects[i][1];
        }

        average1 /= n;
        average2 /= n;

        for (int i = 0; i < n; i++) {
            objects[i][0] -= average1;
            objects[i][1] -= average2;
        }

        double numerator = 0;
        for (int i = 0; i < n; i++) {
            numerator += objects[i][0] * objects[i][1];
        }

        double denominator1 = 0;
        double denominator2 = 0;
        for (int i = 0; i < n; i++) {
            denominator1 += objects[i][0] * objects[i][0];
            denominator2 += objects[i][1] * objects[i][1];
        }

        double denominator = denominator1 * denominator2;

        if (denominator == 0) {
            System.out.println(0);
        } else {
            System.out.println(numerator / Math.sqrt(denominator));
        }
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
}