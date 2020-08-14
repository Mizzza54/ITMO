import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Scanner;

/**
 * @author Michale Gerasimov
 * start: 14.12.2019
 * @version -
 */
public class Num2Choose {
    final static BigInteger[] factorial = new BigInteger[31];

    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("num2choose.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("num2choose.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        int n, k;
        long m;
        makeFactorials(factorial);
        n = scanner.nextInt();
        k = scanner.nextInt();
        m = scanner.nextLong();
        ArrayList<Integer> choose = new ArrayList<>();
        choose = num2choose(n, k, m);
        for (int i = 0; i < k; i++) {
            writer.print(choose.get(i) + " ");
        }
        writer.close();
    }

    public static ArrayList<Integer> num2choose(int n, int k, long m) {
        ArrayList<Integer> choose = new ArrayList<Integer>();
        int next = 1;
        while (k > 0 && n >= 0) {
            if (m < SizeOfChoose(n-1, k - 1)) {
                choose.add(next);
                k--;
            } else {
                m -= SizeOfChoose(n - 1, k - 1);
            }
            n--;
            next++;
        }
        return choose;
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
