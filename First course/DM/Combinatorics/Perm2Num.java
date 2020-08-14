import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Scanner;

/**
 * @author Michale Gerasimov
 * start: 14.12.2019
 * @version -
 */
public class Perm2Num {
    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("perm2num.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("perm2num.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    final static long[] sizeOfPerm = {
            1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, 39916800,
            479001600, 6227020800L, 87178291200L, 1307674368000L,
            20922789888000L, 355687428096000L, 6402373705728000L };
    public static boolean[] was;
    public static int n;

    public static void main(String[] args) {
        n = scanner.nextInt();
        ArrayList<Integer> str = new ArrayList<>();
        str.add(-1);
        for (int i = 0; i < n; i++) {
            str.add(scanner.nextInt());
        }
        was = new boolean[n + 1];
        writer.print(perm2num(str));
        writer.close();
    }

    public static long perm2num (ArrayList<Integer> a){
       long numOfPerm = 0;
       for (int i = 1; i < n + 1; i++) {
           for (int j = 1; j < a.get(i); j++) {
               if (was[j] == false) {
                   numOfPerm += sizeOfPerm[n - i];
               }
           }
           was[a.get(i)] = true;
       }
       return numOfPerm;
    }
}
