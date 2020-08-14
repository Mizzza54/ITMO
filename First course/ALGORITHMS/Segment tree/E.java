
import java.io.*;
import java.util.StringTokenizer;

/**
 * @author Michale Gerasimov
 * start: 24.02.2020
 * @version -
 */
public class E {
    final static int N = 1 << 18;
    public static int r, sizeTree;

    public static class Matrix {
        public int x11, x12, x21, x22;
        Matrix(int x11, int x12, int x21, int x22) {
            this.x11 = x11;
            this.x12 = x12;
            this.x21 = x21;
            this.x22 = x22;
        }
    }

    public static Matrix E = new Matrix(1, 0, 0, 1);
    public static Matrix[] t;

    public static void main(String[] args) throws FileNotFoundException {
        FastReader scanner = new FastReader("crypto.in");
        PrintWriter writer = new PrintWriter("crypto.out");
        int n, m, len;
        r = scanner.nextInt();
        n = scanner.nextInt();
        m = scanner.nextInt();

        sizeTree = 1;
        while (sizeTree < n) {
            sizeTree *= 2;
        }
        t = new Matrix[2 * sizeTree - 1];

        int x11, x22, x12, x21;
        for (int i = sizeTree - 1; i < sizeTree - 1 + n; i++) {
            x11 = scanner.nextInt();
            x12 = scanner.nextInt();
            x21 = scanner.nextInt();
            x22 = scanner.nextInt();
            t[i] = new Matrix(x11, x12, x21, x22);
        }

        for (int i = sizeTree - 1 + n; i < sizeTree * 2 - 1; i++) {
            t[i] = E;
        }

        for (int i = sizeTree - 2; i >= 0; i--) {
            t[i] = multiply(t[2 * i + 1], t[2 * i + 2]);
        }

        int a, b;
        for (int i = 0; i < m; i++) {
            a = scanner.nextInt();
            a--;
            b = scanner.nextInt();
            Matrix result = result(0, 0, sizeTree, a, b);
            writer.println(result.x11 + " " + result.x12 + "\n" + result.x21 + " " + result.x22 + "\n");
        }
        writer.close();

    }

    public static Matrix multiply (Matrix A, Matrix B) {
        return new Matrix(((A.x11 * B.x11) % r + (A.x12 * B.x21) % r) % r,
                ((A.x11 * B.x12) % r + (A.x12 * B.x22) % r) % r,
                ((A.x21 * B.x11) % r + (A.x22 * B.x21) % r) % r,
                ((A.x21 * B.x12) % r + (A.x22 * B.x22) % r) % r);
    }

    public static Matrix result(int v, int l, int r, int a, int b) {
        if (l >= a && r <= b) {
            return t[v];
        }
        if (l >= b || r <= a) {
            return E;
        }
        int m = (l + r) / 2;
        Matrix lres = result(2 * v + 1, l, m, a, b);
        Matrix rres = result(2 * v + 2, m, r, a, b);
        return multiply(lres, rres);
    }

    public static class FastReader {
        BufferedReader br;
        StringTokenizer st;

        FastReader(String name) throws FileNotFoundException {
            br = new BufferedReader(new
                    InputStreamReader(new
                    FileInputStream(name)));
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
    }
}
