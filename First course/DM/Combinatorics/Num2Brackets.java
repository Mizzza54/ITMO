import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Scanner;

/**
 * @author Michale Gerasimov
 * start: 16.12.2019
 * @version -
 */
public class Num2Brackets {
    public static long[][] sizeOfBrackets;

    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("num2brackets.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("num2brackets.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        int n;
        long k;
        n = scanner.nextInt();
        k = scanner.nextLong();
        if (k == 0) {
            for (int i = 0; i < n; i++) {
                writer.print("(");
            }
            for (int i = 0; i < n; i++) {
                writer.print(")");
            }
        } else {
            k++;
            makeSizeOfBrackets(n);
            writer.print(getSequence(n, k));
        }
        writer.close();
    }

    public static String getSequence (int n,long k) {
        int depth = 0;
        String sequence = "";
        for (int i = 0; i < 2 * n; i++) {
            if (sizeOfBrackets[2 * n - (i + 1)][depth + 1] >= k) {
                sequence += "(";
                depth++;
            } else {
                k -= sizeOfBrackets[2 * n - (i + 1)][depth + 1];
                sequence += ")";
                depth--;
            }
        }
        return sequence;
    }

    private static void makeSizeOfBrackets(int n) {
        sizeOfBrackets = new long[n * 2 + 1][n + 1];
        for (int i = 1; i < n + 1; i++) {
            sizeOfBrackets[0][i] = 0;
        }
        sizeOfBrackets[0][0] = 1;
        for (int i = 1; i < 2 * n + 1; i++) {
            for (int j = 0; j < n + 1; j++) {
                if (j > 0)
                    sizeOfBrackets[i][j] += sizeOfBrackets[i - 1][j - 1];
                if (j < n)
                    sizeOfBrackets[i][j] += sizeOfBrackets[i - 1][j + 1];
            }
        }
    }
}
