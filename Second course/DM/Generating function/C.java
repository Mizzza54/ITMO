import java.io.*;
import java.util.Arrays;

/**
 * @author Michael Gerasimov
 * start: 13.04.2021
 * @version -
 */
public class C {
    StreamTokenizer scanner = new StreamTokenizer(new BufferedReader(new InputStreamReader(System.in)));
    PrintWriter writer = new PrintWriter(System.out);

    public static void main(String[] args) {
        C C = new C();
        C.run();
    }

    public void run() {
        int k;
        long[] beginValues, coefficients;
        k = nextInt();
        beginValues = new long[k];
        coefficients = new long[k];
        for (int i = 0; i < k; i++) {
            beginValues[i] = nextInt();
        }
        for (int i = 0; i < k; i++) {
            coefficients[i] = nextInt();
        }

        Polynomial denominator = buildDenominator(coefficients);
        Polynomial numerator = buildNumerator(beginValues, denominator);

        int degree = k;
        for (int i = k - 1; i > -1; i--) {
            degree--;
            if (numerator.getElement(i) != 0) {
                break;
            }
        }

        writer.write(Integer.toString(degree));
        writer.write("\n");
        for (int i = 0; i < degree + 1; i++) {
            writer.write(Long.toString(numerator.array[i]));
            writer.write(" ");
        }
        writer.write("\n");

        writer.write(Integer.toString(k));
        writer.write("\n");
        for (int i = 0; i < k + 1; i++) {
            writer.write(Long.toString(denominator.array[i]));
            writer.write(" ");
        }
        writer.write("\n");

        writer.flush();
        writer.close();
    }

    public Polynomial buildDenominator(long[] coefficients) {
        long[] inversionCoefficients = new long[coefficients.length + 1];
        inversionCoefficients[0] = 1;
        for (int i = 0; i < coefficients.length; i++) {
            inversionCoefficients[i + 1] = coefficients[i] * (-1);
        }
        return new Polynomial(inversionCoefficients);
    }

    public Polynomial buildNumerator(long[] beginValues, Polynomial denominator) {
        Polynomial beginValuesPolynomial = new Polynomial(beginValues);
        return Polynomial.multiply(denominator, beginValuesPolynomial);
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
    static final int MAX_ARRAY_SIZE = 1002;

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

    public static Polynomial subtraction(Polynomial x, Polynomial y) {
        long[] result = new long[MAX_ARRAY_SIZE];
        for (int i = 0; i < result.length; i++) {
            result[i] = x.getElement(i) - y.getElement(i);
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

    public static Polynomial divide(Polynomial x, Polynomial y) {
        long[] result = new long[MAX_ARRAY_SIZE];
        for (int i = 0; i < result.length; i++) {
            long sum = 0;
            for (int j = 0; j < i; j++) {
                sum = result[j] * y.getElement(i - j);
            }
            result[i] = x.getElement(i) - sum / y.getElement(0);
        }
        return new Polynomial(result);
    }

    public static Polynomial pow(Polynomial x, long n) {
        if (n == 0) {
            return new Polynomial(1L);
        }
        Polynomial result = x;
        for (int i = 2; i <= n; i++) {
            result = multiply(result, x);
        }
        return result;
    }

    public Polynomial shift(int n) {
        long[] shift = new long[n + 1];
        shift[0] = (1 / this.getElement(0));
        for (int i = 1; i <= n; i++) {
            long k = 0;
            for (int j = 1; j <= i; j++) {
                k = k + this.getElement(j) * shift[i - j];
            }
            shift[i] = -k / this.getElement(0);
        }
        return new Polynomial(shift);
    }

    @Override
    public String toString() {
        return Arrays.toString(array);
    }
}