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
public class Choose2Num {
    final static BigInteger[] factorial = new BigInteger[31];

    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("choose2num.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("choose2num.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        int n, k;
        makeFactorials(factorial);
        n = scanner.nextInt();
        k = scanner.nextInt();
        int[] choose = new int[k + 1];
        choose[0] = 0;
        for (int i = 1; i < k + 1; i++) {
            choose[i] = scanner.nextInt();
        }
        writer.print(choose2num(choose, n, k));
        writer.close();
    }

    public static long choose2num (int[] choose, int n, int k) {
        long numOfChoose = 0;
        for (int i = 1; i < k + 1; i++) {
            for (int j = choose[i - 1] + 1; j < choose[i]; j++) {
                numOfChoose += SizeOfChoose(n - j, k - i);
            }
        }
        return numOfChoose;
    }

    public static long SizeOfChoose(int n, int k) {
        if (k > n) {
            return 0;
        } else {
            return Long.parseLong(String.valueOf(factorial[n].divide(factorial[n-k].multiply(factorial[k]))));
        }
    }

    public static void makeFactorials(BigInteger[] factorial) {
        factorial[0] = BigInteger.ONE;
        for (int i = 1; i < factorial.length; i++) {
            factorial[i] = factorial[i - 1].multiply(new BigInteger(i + ""));
        }
    }
}
