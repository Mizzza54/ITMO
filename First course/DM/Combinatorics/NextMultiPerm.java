import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Scanner;

/**
 * @author Michale Gerasimov
 * start: 19.12.2019
 * @version -
 */
public class NextMultiPerm {
    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("nextmultiperm.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("nextmultiperm.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        int n = scanner.nextInt();
        int[] permutation = new int[n];
        for (int i = 0; i < n; i++) {
            permutation[i] = scanner.nextInt();
        }
        permutation = nextMultiPerm(permutation);
        if (permutation == null) {
            for (int i = 0; i < n; i++) {
                writer.print(0 + " ");
            }
        } else {
            for (int i = 0; i < n; i++) {
                writer.print(permutation[i] + " ");
            }
        }
        writer.close();
    }

    public static int[] nextMultiPerm (int[] a) {
        int n = a.length;
        int i = n - 2;
        while (i >= 0 && a[i] >= a[i + 1]) {
            i--;
        }
        if (i >= 0) {
            int j = i + 1;
            while (j < n - 1 && a[j + 1] > a[i]) {
                j++;
            }
            swap(a, i, j);
            reverse(a, i + 1, n - 1);
            return a;
        } else {
            return null;
        }
    }

    private static void swap(int[] a, int i, int j) {
        int tmp = a[i];
        a[i] = a[j];
        a[j] = tmp;
    }

    private static void reverse(int[] a, int l, int r) {
        int left = l, right = r;
        while (left < right)
            swap(a, left++, right--);
    }
}
