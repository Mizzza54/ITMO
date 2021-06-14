import java.io.*;
import java.util.Arrays;

/**
 * @author Michael Gerasimov
 * start: 15.04.2021
 * @version -
 */
public class D {
    StreamTokenizer scanner = new StreamTokenizer(new BufferedReader(new InputStreamReader(System.in)));
    PrintWriter writer = new PrintWriter(System.out);

    public static void main(String[] args) {
        D D = new D();
        D.run();
    }

    public void run() {
        int r, k;
        r = nextInt();
        k = nextInt();
        long[] numeratorArray = new long[k + 1];
        for (int i = 0; i < k + 1; i++) {
            numeratorArray[i] = nextInt();
        }
        Polynomial numerator = new Polynomial(numeratorArray);

        long[] powOfR = new long[11];
        powOfR[0] = 1;
        for (int i = 1; i < 11; i++) {
            powOfR[i] = r * powOfR[i - 1];
        }

        Polynomial sum = new Polynomial(0);
        for (int i = 0; i < k + 1; i++) {
            Polynomial denominator = new Polynomial(1);

            for (int j = 1; j <= k; j++) {

                Polynomial member = new Polynomial(new long[]{j - i, 1});
                denominator = Polynomial.multiply(denominator, member);
            }

            long coefficient = powOfR[k - i] * numerator.getElement(i);

            for (int j = 0; j < denominator.size(); j++) {
                denominator.array[j] *= coefficient;
            }

            sum = Polynomial.add(sum, denominator);
        }

        long division = factorial(k) * powOfR[k];

        for (int i = 0; i < k + 1; i++) {
            long nod = gcd(Math.abs(sum.array[i]), division);
            writer.write(sum.array[i] / nod + "/" + division / nod + " ");
        }

        writer.flush();
        writer.close();
    }

    long factorial(long n) {
        long result = 1;
        for (int i = 1; i <= n; i++) {
            result *= i;
        }
        return result;
    }

    long gcd(long x, long y) {
        if (x == y) {
            return x;
        }

        if (x == 0) {
            return y;
        }

        if (y == 0) {
            return x;
        }

        if (x % 2 == 0) {
            if (y % 2 == 1) {
                return gcd(x / 2, y);
            } else {
                return 2 * gcd(x / 2, y / 2);
            }
        } else {
            if (y % 2 == 0) {
                return gcd(x, y / 2);
            }

            if (x > y) {
                return gcd((x - y) / 2, y);
            } else {
                return gcd((y - x) / 2, x);
            }
        }
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

class Polynomial {
    long[] array;
    static final int MAX_ARRAY_SIZE = 102;

    Polynomial(long[] array) {
        int size = Math.min(MAX_ARRAY_SIZE, array.length);
        this.array = new long[MAX_ARRAY_SIZE];
        System.arraycopy(array, 0, this.array, 0, size);
    }

    Polynomial(long number) {
        this.array = new long[]{number};
    }

    public Long getElement(int index) {
        return index >= array.length ? 0L : array[index];
    }

    public int size() {
        return array.length;
    }

    public static Polynomial add(Polynomial x, Polynomial y) {
        long[] result = new long[MAX_ARRAY_SIZE];
        for (int i = 0; i < result.length; i++) {
            result[i] = x.getElement(i) + y.getElement(i);
        }
        return new Polynomial(result);
    }

    public static Polynomial multiply(Polynomial x, Polynomial y) {
        long[] result = new long[MAX_ARRAY_SIZE];
        for (int i = 0; i < MAX_ARRAY_SIZE; i++) {
            for (int j = 0; j < MAX_ARRAY_SIZE - i; j++) {
                result[i + j] += x.getElement(i) * y.getElement(j);
            }
        }
        return new Polynomial(result);
    }

    @Override
    public String toString() {
        return Arrays.toString(array);
    }
}