import java.io.*;
import java.util.Arrays;

/**
 * @author Michael Gerasimov
 * start: 13.04.2021
 * @version -
 */
public class I {
    StreamTokenizer scanner = new StreamTokenizer(new BufferedReader(new InputStreamReader(System.in)));

    public static void main(String[] args) {
        I I = new I();
        I.run();
    }

    public void run() {
        long n;
        int k;
        long[] beginValues, coefficients;
        k = nextInt();
        n = nextLong() - 1;
        beginValues = new long[2 * k + 1];
        coefficients = new long[k + 1];
        for (int i = 0; i < k; i++) {
            beginValues[i] = nextInt();
        }

        coefficients[0] = 1;
        for (int i = 1; i < k + 1; i++) {
            coefficients[i] = (nextInt() * (-1) + Polynomial.MOD) % Polynomial.MOD;
        }

        Polynomial numerator = new Polynomial(beginValues);
        Polynomial denominator = new Polynomial(coefficients);
        Polynomial result = null;
        Polynomial inversionDenominator = null;

        System.out.println(getNth(n, k, numerator, denominator, result, inversionDenominator));
    }

    public long getNth(long n, int k, Polynomial numerator, Polynomial denominator, Polynomial result, Polynomial inversionDenominator) {
        while (n >= k) {
            for (int i = k; i <= 2 * k - 1; i++) {
                numerator.array[i] = 0;
                for (int j = 1; j <= k; j++) {
                    numerator.array[i] = Polynomial.subtraction(numerator.getElement(i),
                            Polynomial.multiply(denominator.getElement(j), numerator.getElement(i - j)));
                }
            }


            inversionDenominator = new Polynomial(denominator.array, true);
            result = Polynomial.multiply(denominator, inversionDenominator);
            for (int i = (int) Polynomial.NormalizeValue(n % 2); i < 2 * k; i += 2) {
                numerator.array[i / 2] = numerator.getElement(i);
            }

            for (int i = 0; i <= k; i++) {
                denominator.array[i] = result.getElement(i * 2);
            }
            n /= 2;
        }
        return numerator.getElement((int) n);
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

    long nextLong() {
        try {
            scanner.nextToken();
            return (long) scanner.nval;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return -42;
    }
}

class Polynomial {
    long[] array;
    static long MOD = 104_857_601;

    Polynomial(long[] array) {
        int size = array.length;
        this.array = new long[size];
        for (int i = 0; i < size; i++) {
            this.array[i] = NormalizeValue(array[i]);
        }
    }

    Polynomial(long[] array, boolean isInversion) {
        if (!isInversion) {
            new Polynomial(array);
        } else {
            int size = array.length;
            this.array = new long[size];
            for (int i = 0; i < array.length; i++) {
                this.array[i] = i % 2 == 0 ? NormalizeValue(array[i]) : NormalizeValue(-array[i]);
            }
        }
    }

    public Long getElement(int index) {
        return index >= array.length ? 0L : array[index];
    }

    public int size() {
        return array.length;
    }

    public static Polynomial multiply(Polynomial x, Polynomial y) {
        long[] result = new long[x.size() + y.size() + 1];
        for (int i = 0; i < result.length; i += 2) {
            for (int j = 0; j < i + 1; j++) {
                result[i] = add(result[i], multiply(x.getElement(j), y.getElement(i - j)));
            }
        }
        return new Polynomial(result);
    }

    public static long add(long x, long y) {
        return NormalizeValue(x + y);
    }

    public static long subtraction(long x, long y) {
        return NormalizeValue(x - y);
    }

    public static long multiply(long x, long y) {
        return NormalizeValue(x * y);
    }

    public static long NormalizeValue(long x) {
        return x % MOD + ((x < 0) ? MOD : 0);
    }

    @Override
    public String toString() {
        return Arrays.toString(array);
    }
}