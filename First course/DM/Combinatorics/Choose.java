
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Scanner;

/**
 * @author Michale Gerasimov
 * start: 14.12.2019
 * @version -
 */
public class Choose {
    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("choose.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("choose.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        int n, k;
        n = scanner.nextInt();
        k = scanner.nextInt();
        int[] combination = new int[k];
        for (int i = 0; i < k; i++) {
            combination[i] = i + 1;
            writer.print(combination[i] + " ");
        }
        writer.println();
        combination = nextChoose(combination, n ,k);
        while (combination != null) {
            for (int i = 0; i < k; i++) {
                writer.print(combination[i] + " ");
            }
            writer.println();
            combination = nextChoose(combination, n, k);
        }
        writer.close();
    }

    public static int[] nextChoose(int[] a, int n, int k) {
        int[] tmp = new int[k+1];
        int index;
        for (int i = 0; i < k; i++) {
            tmp[i] = a[i];
        }
        tmp[k] = n + 1;
        index = k - 1;
        while (index >= 0 && tmp[index + 1] - tmp[index] < 2) {
            index--;
        }
        if (index >= 0) {
            tmp[index]++;
            for (int i = index + 1; i < k; i++) {
                tmp[i] = tmp[i - 1] + 1;
            }
            for (int i = 0; i < k; i++) {
                a[i] = tmp[i];
            }
            return a;
        } else {
            return null;
        }
    }
}