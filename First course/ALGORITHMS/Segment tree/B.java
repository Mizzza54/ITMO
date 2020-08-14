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
public class B {
    public static FastReader scanner = new FastReader(System.in);
    public static long[] t;
    public static int id;
    public static void main(String[] args) {
        int n = scanner.nextInt();
        int sizeTree = (int) (Math.log(n) / Math.log(2));
        sizeTree++;
        int[] data = new int[(int) Math.pow(2, sizeTree)];
        for (int i = 0; i < n; i++) {
            data[i] = scanner.nextInt();
        }
        for (int i = n; i < data.length; i++) {
            data[i] = 0;
        }
        sizeTree = (int) (Math.pow(2, sizeTree + 1));
        id = sizeTree / 2 - 1;
        sizeTree--;
        t = new long[sizeTree];
        build(data, 0, 0, data.length);

        String line = "qwerty";
        line = scanner.nextLine();
        while (line != null) {
            int i = 4;
            while (Character.isDigit(line.charAt(i))) {
                i++;
            }
            int a = Integer.parseInt(line.substring(4, i)) - 1;
            i++;
            int b = Integer.parseInt(line.substring(i, line.length()));
            if (line.charAt(1) == 'u') {
                System.out.println(sum(0,0, data.length, a, b));
            }
            if (line.charAt(1) == 'e') {
                set(a, b);
            }
            line = scanner.nextLine();
        }
    }

    public static void build (int[] a, int v, int l, int r) {
        if (r - l == 1) {
            t[v] = a[l];
        } else {
            int m = (l + r) / 2;
            build(a, 2 * v + 1, l, m);
            build(a, 2 * v + 2, m, r);
            t[v] = t[2 * v + 1] + t[2 * v + 2];
        }
    }

    public static void set(int i, int x) {
        t[id + i] = x;
        int j = id + i;
        while (j > 0) {
            j = (j - 1) / 2;
            t[j] = t[2 * j + 1] + t[2 * j + 2];
        }
    }

    public static long sum(int v, int l, int r, int a, int b) {
        if (l >= a && r <= b) {
            return t[v];
        }
        if (l >= b || r <= a) {
            return 0;
        }
        int m = (l + r) / 2;
        return sum(2 * v + 1, l, m, a, b) + sum(2 * v + 2, m, r, a, b);
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
