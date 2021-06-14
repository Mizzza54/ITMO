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
public class E {
    PrintWriter writer = new PrintWriter(System.out);
    FastReader scanner = new FastReader(System.in);

    public static void main(String[] args) {
        E E = new E();
        E.run();
    }

    public void run() {
        int n = scanner.nextInt();
        int[] a = new int[n + 1];
        int[] b = new int[n + 1];
        for (int i = 0; i < n + 1; i++) {
            a[i] = scanner.nextInt();
        }
        for (int i = 0; i < n + 1; i++) {
            b[i] = scanner.nextInt();
        }
        int[] result = multiply(a, b);
        StringBuilder stringBuilder = new StringBuilder();
        for (int i = 0; i < 2 * n + 1; i++) {
            stringBuilder.append(result[i]);
            stringBuilder.append(" ");
        }
        writer.write(stringBuilder.toString());
        writer.close();
    }

    private void fft (double[] re, double[] im, boolean invert) {
        int n = re.length;
        if (n == 1) {
            return;
        }
        double[] part1Re = new double[n / 2];
        double[] part1Im = new double[n / 2];
        double[] part2Re = new double[n / 2];
        double[] part2Im = new double[n / 2];
        for (int i = 0, j = 0; i < n; i += 2, j++) {
            part1Re[j] = re[i];
            part1Im[j] = im[i];
            part2Re[j] = re[i + 1];
            part2Im[j] = im[i + 1];
        }

        fft (part1Re, part1Im, invert);
        fft (part2Re, part2Im, invert);

        double angle = 2 * Math.PI / n * (invert ? -1 : 1);
        double wRe = 1;
        double wIm = 0;
        double wnRe = Math.cos(angle);
        double wnIm = Math.sin(angle);

//        if (!invert && n == 8) {
//            System.out.println(Arrays.toString(part1Re));
//            System.out.println(Arrays.toString(part1Im));
//        }


        for (int i = 0; i < n / 2; i++) {
            re[i] = (wRe * part2Re[i] - wIm * part2Im[i]) + part1Re[i];
            im[i] = (wRe * part2Im[i] + wIm * part2Re[i]) + part1Im[i];

            re[i+n/2] = part1Re[i] - (wRe * part2Re[i] - wIm * part2Im[i]);
            im[i+n/2] = part1Im[i] - (wRe * part2Im[i] + wIm * part2Re[i]);

            if (invert) {
                re[i] /= 2;
                im[i] /= 2;
                re[i+n/2] /= 2;
                im[i+n/2] /= 2;
            }

            double temp = wRe * wnRe - wIm * wnIm;
            wIm = wRe * wnIm + wIm * wnRe;
            wRe = temp;
//            System.out.println(wRe);
//            System.out.println(wIm);
        }
    }

    private int[] multiply(int[] a, int[] b) {
        int n = 1;
        while (n < a.length) {
            n <<= 1;
        }
        n <<= 1;
        double[] faRe = new double[n];
        double[] faIm = new double[n];
        double[] fbRe = new double[n];
        double[] fbIm = new double[n];

        for (int i = 0; i < a.length; i++) {
            faRe[i] = a[i];
            fbRe[i] = b[i];
        }

        fft(faRe, faIm, false);
        fft(fbRe, fbIm, false);

        for (int i = 0; i < n; i++) {
            double temp = faRe[i] * fbRe[i] - faIm[i] * fbIm[i];
            faIm[i] = faRe[i] * fbIm[i] + faIm[i] * fbRe[i];
            faRe[i] = temp;
        }
        fft (faRe, faIm, true);

        int[] result = new int[n];
        for (int i = 0; i < n; i++) {
            result[i] = (int) (faRe[i] + 0.5);
        }
        return result;
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
}