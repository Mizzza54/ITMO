import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 28.05.2021
 * @version -
 */
public class F {
    PrintWriter writer = new PrintWriter(System.out);
    FastReader scanner = new FastReader(System.in);

    public static void main(String[] args) {
        F F = new F();
        F.run();
    }

    public void run() {
        String string = scanner.next();
        int[] array1 = new int[string.length()];
        int[] array2 = new int[string.length()];
        for (int i = 0; i < string.length(); i++) {
            switch (string.charAt(i)) {
                case '0':
                    array1[i] = 0;
                    array2[i] = 0;
                    break;
                case '1':
                    array1[i] = 1;
                    array2[i] = 1;
                    break;
            }

        }
        int[] result = multiply(array1, array2);
        long count = 0;
        for (int i = 0; i < string.length(); i++) {
            if (string.charAt(i) == '1') {
                count += result[2 * i] / 2;
            }
        }
        writer.println(count);
        writer.flush();
        writer.close();
    }

    private ComplexNumber[] fft (ComplexNumber[] array, boolean invert) {
        int n = array.length;
        if (n == 1) {
            return array;
        }

        ComplexNumber[] part1 = new ComplexNumber[n / 2];
        ComplexNumber[] part2 = new ComplexNumber[n / 2];
        for (int i = 0, j = 0; i < n; i += 2, j++) {
            part1[j] = array[i];
            part2[j] = array[i+1];
        }
        fft (part1, invert);
        fft (part2, invert);

        double ang = 2 * Math.PI / n * (invert ? -1 : 1);
        ComplexNumber w  = new ComplexNumber(1, 0);
        ComplexNumber wn = new ComplexNumber(Math.cos(ang), Math.sin(ang));
        for (int i = 0; i < n / 2; i++) {
            array[i] = ComplexNumber.sum(part1[i], ComplexNumber.multiply(w, part2[i]));
            array[i + n / 2] = ComplexNumber.subtract(part1[i], ComplexNumber.multiply(w, part2[i]));
            if (invert) {
                array[i] = ComplexNumber.divide(array[i], new ComplexNumber(2, 0));
                array[i + n / 2] = ComplexNumber.divide(array[i + n / 2], new ComplexNumber(2, 0));
            }
            w = ComplexNumber.multiply(w, wn);
        }
        return array;
    }

    private int[] multiply(int[] a, int[] b) {
        int n = 1;
        while (n < Math.max(a.length, b.length)) {
            n <<= 1;
        }
        n <<= 1;
        ComplexNumber[] fa = new ComplexNumber[n];
        ComplexNumber[] fb = new ComplexNumber[n];
        for (int i = 0; i < a.length; i++) {
            fa[i] = new ComplexNumber(a[i], 0);
        }
        for (int i = a.length; i < n; i++) {
            fa[i] = new ComplexNumber(0, 0);
        }
        for (int i = 0; i < b.length; i++) {
            fb[i] = new ComplexNumber(b[i], 0);
        }
        for (int i = b.length; i < n; i++) {
            fb[i] = new ComplexNumber(0, 0);
        }

        fft(fa, false);
        fft(fb, false);
        for (int i = 0; i < n; i++) {
            fa[i] = ComplexNumber.multiply(fa[i], fb[i]);
        }
        fft (fa, true);

        int[] result = new int[n];
        for (int i = 0; i < n; i++) {
            result[i] = (int) (fa[i].getRe() + 0.5);
        }
        return result;
    }
}

class ComplexNumber {
    private final double re;
    private final double im;

    public ComplexNumber(double re, double im) {
        this.re = re;
        this.im = im;
    }

    public ComplexNumber() {
        this.re = 0;
        this.im = 0;
    }

    public double getRe() {
        return re;
    }

    public double getIm() {
        return im;
    }

    public static ComplexNumber sum(ComplexNumber cn1, ComplexNumber cn2) {
        return new ComplexNumber(cn1.getRe() + cn2.getRe(), cn1.getIm() + cn2.getIm());
    }

    public static ComplexNumber multiply(ComplexNumber cn1, ComplexNumber cn2) {
        return new ComplexNumber(cn1.getRe() * cn2.getRe() - cn1.getIm() * cn2.getIm(), cn1.getRe() * cn2.getIm() + cn1.getIm() * cn2.getRe());
    }

    public static ComplexNumber subtract(ComplexNumber cn1, ComplexNumber cn2) {
        return new ComplexNumber(cn1.getRe() - cn2.getRe(), cn1.getIm() - cn2.getIm());
    }

    public static ComplexNumber divide(ComplexNumber cn1, ComplexNumber cn2) {
        ComplexNumber temp = new ComplexNumber(cn2.getRe(), (-1) * cn2.getIm());
        temp = ComplexNumber.multiply(cn1, temp);
        double denominator = cn2.getRe() * cn2.getRe() + cn2.getIm() * cn2.getIm();
        return new ComplexNumber(temp.getRe() / denominator, temp.getIm() / denominator);
    }

    @Override
    public String toString() {
        return "ComplexNumber{" +
                "re=" + re +
                ", im=" + im +
                '}';
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