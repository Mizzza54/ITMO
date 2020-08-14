import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Scanner;

/**
 * @author Michale Gerasimov
 * start: 17.12.2019
 * @version -
 */
public class NextChoose {
    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("nextchoose.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("nextchoose.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        int n, k;
        n = scanner.nextInt();
        k = scanner.nextInt();
        scanner.nextLine();
        int[] combination = new int[k];
        String[] line = scanner.nextLine().split(" ");
        for (int i = 0; i < k; i++) {
            combination[i] = Integer.parseInt(line[i]);
        }
        combination = nextChoose(combination, n, k);
        if (combination == null) {
            writer.print(-1);
        } else {
            for (int i = 0; i < k; i++) {
                writer.print(combination[i] + " ");
            }
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