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
public class D {
    PrintWriter writer = new PrintWriter(System.out);
    FastReader scanner = new FastReader(System.in);

    public static void main(String[] args) {
        D D = new D();
        D.run();
    }

    public void run() {
        long n, e, c;
        n = scanner.nextLong();
        e = scanner.nextLong();
        c = scanner.nextLong();
        long p1 = 0, p2 = 0;
        for (int i = 2; (long) i * i < n + 1; i++) {
            if (n % i == 0) {
                p1 = i;
                p2 = n / i;
            }
        }
        long mod = (p1 - 1) * (p2 - 1);
        long d = gcd(e, mod).y;
        d = d < 0 ? d + mod : d;
        long m = fastPow(c, d, n);
        writer.println(m);
        writer.flush();
        writer.close();
    }

    private long fastPow(long number, long degree, long mod) {
        if (degree == 1) return number % mod;
        if (degree % 2 == 0) {
            long half = fastPow(number, degree / 2, mod);
            return modMultiply(half, half, mod);
        }
        return modMultiply(number, fastPow(number, degree - 1, mod), mod);
    }

    private long modMultiply(long a, long b, long mod) {
        long result = 0;
        while (b != 0) {
            if (b % 2 == 1) {
                result = (result + a) % mod;
            }
            a = (2 * a) % mod;
            b /= 2;
        }
        return result;
    }

    private Result gcd(long a, long b) {
        if (a == 0) {
            return new Result(b, 0, 1);
        }

        Result result = gcd(b % a, a);
        return new Result(result.x, result.z - (b / a) * result.y, result.y);
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

class Result {
    long x, y, z;

    public Result(long x, long y, long z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }
}