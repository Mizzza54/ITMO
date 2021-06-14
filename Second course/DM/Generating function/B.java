import java.io.*;
import java.util.Arrays;

/**
 * @author Michael Gerasimov
 * start: 11.04.2021
 * @version -
 */
public class B {
    StreamTokenizer scanner = new StreamTokenizer(new BufferedReader(new InputStreamReader(System.in)));
    PrintWriter writer = new PrintWriter(System.out);

    public static void main(String[] args) {
        B B = new B();
        B.run();
    }

    public void run() {
        int n, m;
        Polynomial polynomial;
        long[] coefficient;
        n = nextInt();
        m = nextInt();
        coefficient = new long[n + 1];
        for (int i = 0; i < n + 1; i++) {
            coefficient[i] = nextInt();
        }
        polynomial = new Polynomial(coefficient);

        // SQRT
        Polynomial polynomialSqrt = new Polynomial(1);
        Polynomial polynomialExp = new Polynomial(1);
        Polynomial polynomialLn = new Polynomial(polynomial.array);
        for (int i = 1; i < m; i++) {
            long constanta = Polynomial.divide(factorial(2L * i),
                    Polynomial.multiply(((1 - 2L * i) % Polynomial.MOD + Polynomial.MOD),
                            Polynomial.multiply(factorial(i),
                                    Polynomial.multiply(factorial(i), power4(i)))));
            polynomialSqrt = i % 2 != 0
                    ? Polynomial.subtraction(polynomialSqrt, Polynomial.multiply(new Polynomial(constanta), Polynomial.pow(polynomial, i)))
                    : Polynomial.add(polynomialSqrt, Polynomial.multiply(new Polynomial(constanta), Polynomial.pow(polynomial, i)));
            polynomialExp = Polynomial.add(polynomialExp,
                    Polynomial.divide(Polynomial.pow(polynomial, i), new Polynomial(factorial(i))));
            polynomialLn = i % 2 != 0
                    ? Polynomial.subtraction(polynomialLn, Polynomial.divide(Polynomial.pow(polynomial, i + 1), new Polynomial(i + 1)))
                    : Polynomial.add(polynomialLn, Polynomial.divide(Polynomial.pow(polynomial, i + 1), new Polynomial(i + 1)));

        }

        polynomialSqrt.toStringForAnswer(m, writer);
        polynomialExp.toStringForAnswer(m, writer);
        polynomialLn.toStringForAnswer(m, writer);
        writer.flush();
        writer.close();
    }

    long factorial(long n) {
        long result = 1;
        for (int i = 1; i <= n; i++) {
            result = (result * i + Polynomial.MOD) % Polynomial.MOD;
        }
        return result;
    }

    long power4(long n) {
        long result = 4;
        for(int i = 2; i <= n; i++){
            result = (result * 4 + Polynomial.MOD) % Polynomial.MOD;
        }
        return result;
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
    static long MOD = 998_244_353;
    static final int MAX_ARRAY_SIZE = 102;

    Polynomial(long[] array) {
        int size = Math.min(MAX_ARRAY_SIZE, array.length);
        this.array = new long[MAX_ARRAY_SIZE];
        for (int i = 0; i < size; i++) {
            this.array[i] = array[i];
        }
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
            result[i] = (x.getElement(i) % MOD + y.getElement(i) % MOD) % MOD;
        }
        return new Polynomial(result);
    }

    public static Polynomial subtraction(Polynomial x, Polynomial y) {
        long[] result = new long[MAX_ARRAY_SIZE];
        for (int i = 0; i < result.length; i++) {
            result[i] = NormalizeValue(NormalizeValue(x.getElement(i)) - NormalizeValue(y.getElement(i)));
        }
        return new Polynomial(result);
    }

    public static Polynomial multiply(Polynomial x, Polynomial y) {
        long[] result = new long[MAX_ARRAY_SIZE];
        for (int i = 0; i < MAX_ARRAY_SIZE; i++) {
            for (int j = 0; j < MAX_ARRAY_SIZE - i; j++) {
                result[i + j] += x.getElement(i) * y.getElement(j) + MOD;
                result[i + j] %= MOD;
            }
        }
        return new Polynomial(result);
    }

    public static long bin_pow(long base, long p) {
        if (p == 0) {
            return 1;
        }

        if (p % 2 == 0) {
            long t = bin_pow(base, p / 2);
            return NormalizeValue((t % MOD * t % MOD) % MOD);
        } else {
            return NormalizeValue(bin_pow(base, p - 1)*base);
        }
    }


    public static long inverse_element(long x) {
        return bin_pow(x, MOD - 2);
    }
    
    public static Polynomial divide(Polynomial x, Polynomial y) {
        long[] result = new long[MAX_ARRAY_SIZE];
        for (int i = 0; i < result.length; i++) {
            long sum = 0;
            for (int j = 0; j < i; j++) {
               sum = (result[j] * y.getElement(i - j) + MOD) % MOD;
            }
            result[i] = divide(x.getElement(i) - sum, y.getElement(0));
        }
        return new Polynomial(result);
    }

    public static Polynomial pow(Polynomial x, long n){
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
                k = (k + this.getElement(j) * shift[i - j] + MOD) % MOD;
            }
            shift[i] = ((-k / this.getElement(0) + MOD) % MOD);
        }
        return new Polynomial(shift);
    }

    public static long add(long x, long y){
        return (x % MOD + y % MOD) % MOD;
    }

    public static long subtraction(long x, long y){
        return (x % MOD - y % MOD) % MOD;
    }

    public static long multiply(long x, long y){
        return NormalizeValue(x * y);
    }

    public static long divide(long x, long y){
        return NormalizeValue((x * inverse_element(y)));
    }

    public static long NormalizeValue(long x) {
        return x % MOD + ((x < 0) ? MOD : 0);
    }

    @Override
    public String toString() {
        return Arrays.toString(array);
    }

    public void toStringForAnswer(int m, PrintWriter writer) {
        for (int i = 0; i < m; i++) {
            writer.write(Long.toString(array[i]));
            writer.write(" ");
        }
        writer.write("\n");
    }
}