import javax.swing.plaf.IconUIResource;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Scanner;

/**
 * @author Michale Gerasimov
 * start: 12.12.2019
 * @version -
 */
public class Num2Perm {
    final static long[] factorials = {
            1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, 39916800,
            479001600, 6227020800L, 87178291200L, 1307674368000L,
            20922789888000L, 355687428096000L, 6402373705728000L };

    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("num2perm.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("num2perm.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        int n = scanner.nextInt();
        long k = scanner.nextLong();
        k++;
        int[] permutation = new int[n];
        boolean[] was = new boolean[n+1];
        long alreadyWas, curFree;
        for (int i = 0; i < n; i++) {
            alreadyWas = (k - 1) / factorials[n - i - 1] + 1;
            k = (k - 1) % factorials[n - i - 1] + 1;
            curFree = 0;
            int j;
            for (j = 1; j < n; j++) {
                if (!was[j]) {
                    curFree++;
                }
                if (curFree == alreadyWas) {
                    break;
                }
            }
            permutation[i] = j;
            was[j] = true;
        }
        for (int i = 0; i < n; i++) {
            writer.print(permutation[i] + " ");
        }
        writer.close();
    }
}