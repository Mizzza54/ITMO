//import com.google.common.math.LongMath;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.math.BigInteger;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;

/**
 * @author Michael Gerasimov
 * start: 25.05.2021
 * @version -
 */
public class B {
    FastReader scanner = new FastReader(System.in);
    PrintWriter writer = new PrintWriter(System.out);

    public static void main(String[] args) {
        B B = new B();
        B.run();
    }

    public void run() {
        int n = scanner.nextInt();
        for (int i = 0; i < n; i++) {
            BigInteger number = new BigInteger(String.valueOf(scanner.nextLong()));
            writer.println(number.isProbablePrime(10) ? "YES" : "NO");

        }
        writer.close();
//        for (long i = 0; i < 1000000000000000005L; i++) {
//            if (LongMath.isPrime(i) != test(i)) {
//                System.out.println(i);
//                break;
//            }
//        }
//        while (true) {
//            boolean a = LongMath.isPrime(7340497);
//            boolean b = test(7340497);
//            if (a != b) {
//                System.out.println(a);
//                System.out.println(b);
//                break;
//            }
//            System.out.println(LongMath.isPrime(7340497));
//            System.out.println(test(7340497));
//        }
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

    private boolean test(long number) {
        if (number == 2 || number == 3) {
            return true;
        }
        if (number < 2 || number % 2 == 0) {
            return false;
        }
        long s = number - 1;
        long count = 0;
        while (s % 2 == 0) {
            s /= 2;
            count++;
        }

        for (int i = 0; i < 10; i++) {
            long r = (long) ((Math.random() * ((number - 1 - 2) + 1)) + 2);
            long mod = fastPow(r, s, number);
            if (mod == 1 || mod == number - 1) {
                continue;
            }
            for (int ind = 1; ind < count; ind++) {
                mod = fastPow(mod, 2, number);
                if (mod == 1) {
                    return false;
                }
                if (mod == number - 1) {
                    break;
                }
            }
            if (mod != number - 1) {
                return false;
            }
        }
        return true;
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