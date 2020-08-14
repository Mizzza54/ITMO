import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.math.BigInteger;
import java.util.Scanner;

/**
 * @author Michale Gerasimov
 * start: 16.12.2019
 * @version -
 */
public class Brackets2Num {
    public static BigInteger[][] sizeOfBrackets;

    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("brackets2num.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("brackets2num.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        int n;
        String sequence;
        sequence = scanner.nextLine();
        n = sequence.length() / 2;
        int count = 0;
        for (int i = 0; i < n; i++) {
            if (sequence.charAt(i) == '(') {
                count++;
            }
        }
        if (count == n) {
            writer.print(0);
        } else {
            makeSizeOfBrackets(n);
            writer.print(getNumber(sequence, n));
        }
        writer.close();
    }

    public static BigInteger getNumber(String sequence, int n) {
        BigInteger number = BigInteger.ZERO;
        int depth = 0;
        for (int i = 0; i < 2 * n; i++) {
            if (sequence.charAt(i) == '(') {
                depth++;
            } else {
                number = number.add(sizeOfBrackets[2 * n - (i + 1)][depth + 1]);
                depth--;
            }
        }
        return number;
    }

    private static void makeSizeOfBrackets(int n) {
        sizeOfBrackets = new BigInteger[n * 2 + 1][n + 1];
        for (int i = 0; i < 2 * n + 1; i++) {
            for (int j = 0; j < n + 1; j++) {
                sizeOfBrackets[i][j] = BigInteger.ZERO;
            }
        }
        sizeOfBrackets[0][0] = BigInteger.ONE;
        for (int i = 1; i < 2 * n + 1; i++) {
            for (int j = 0; j < n + 1; j++) {
                if (j > 0)
                    sizeOfBrackets[i][j] = sizeOfBrackets[i][j].add(sizeOfBrackets[i - 1][j - 1]);
                if (j < n)
                    sizeOfBrackets[i][j] = sizeOfBrackets[i][j].add(sizeOfBrackets[i - 1][j + 1]);
            }
        }
    }
}
