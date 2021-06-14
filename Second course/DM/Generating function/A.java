import java.io.*;
import java.util.Arrays;

/**
 * @author Michael Gerasimov
 * start: 05.04.2021
 * @version -
 */
public class A {
    StreamTokenizer scanner = new StreamTokenizer(new BufferedReader(new InputStreamReader(System.in)));
    PrintWriter writer = new PrintWriter(System.out);

    public static void main(String[] args) {
        A A = new A();
        A.run();
    }

    public void run() {
        int n, m;
        Polynomial polynomialA, polynomialB;
        long[] coefficientA, coefficientB;
        n = nextInt();
        m = nextInt();
        coefficientA = new long[n + 1];
        coefficientB = new long[m + 1];
        for (int i = 0; i < n + 1; i++) {
            coefficientA[i] = nextInt();
        }
        for (int i = 0; i < m + 1; i++) {
            coefficientB[i] = nextInt();
        }
        polynomialA = new Polynomial(coefficientA);
        polynomialB = new Polynomial(coefficientB);
        Polynomial polynomialPlus, polynomialMultiply, polynomialDivide;
        polynomialPlus = Polynomial.add(polynomialA, polynomialB);
        polynomialMultiply = Polynomial.multiply(polynomialA, polynomialB);
        polynomialDivide = Polynomial.multiply(polynomialA, polynomialB.shift(1000));

        int degreePlus = polynomialPlus.size() - 1;
        int degreeMultiply = polynomialMultiply.size() - 1;

        while (degreePlus != 0 && polynomialPlus.getElement(degreePlus) == 0) {
            degreePlus--;
        }

        while (degreeMultiply != 0 && polynomialMultiply.getElement(degreeMultiply) == 0) {
            degreeMultiply--;
        }

        writer.write(degreePlus + "\n");
        for (int i = 0; i <= degreePlus; i++) {
            writer.write(polynomialPlus.getElement(i) + " ");
        }
        writer.write("\n");
        writer.write(degreeMultiply + "\n");
        for (int i = 0; i <= degreeMultiply; i++) {
            writer.write(polynomialMultiply.getElement(i) + " ");
        }
        writer.write("\n");
        for (int i = 0; i < 1000; i++) {
            writer.write(polynomialDivide.getElement(i) + " ");
        }
        writer.write("\n");
        writer.flush();
        writer.close();
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

    Polynomial(long[] array) {
        this.array = Arrays.copyOf(array, array.length);
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
        long[] result = new long[Math.max(x.size(), y.size())];
        for (int i = 0; i < result.length; i++) {
            if (i < x.size()) {
                result[i] += x.getElement(i);
            }
            if (i < y.size()) {
                result[i] += y.getElement(i);
            }
            result[i] = (result[i] + MOD) % MOD;
        }
        return new Polynomial(result);
    }

    public static Polynomial multiply(Polynomial x, Polynomial y) {
        long[] result = new long[x.size() * y.size() + 1];
        for (int i = 0; i < x.size(); i++) {
            for (int j = 0; j < y.size(); j++) {
                result[i + j] += x.getElement(i) * y.getElement(j) + MOD;
                result[i + j] %= MOD;
            }
        }
        return new Polynomial(result);
    }

    public static Polynomial pow(Polynomial x, long n){
        if(n == 0) {
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
}
