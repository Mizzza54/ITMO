import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Scanner;

/**
 * @author Michale Gerasimov
 * start: 16.12.2019
 * @version -
 */
public class NextPerm {
    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("nextperm.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("nextperm.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static int n;

    public static void main(String[] args) {
        n = scanner.nextInt();
        scanner.nextLine();
        String[] line = (scanner.nextLine()).split(" ");

        int[] permutation1 = new int[n];
        int[] permutation2 = new int[n];
        for (int i = 0; i < n; i++) {
            permutation1[i] = Integer.parseInt(line[i]);
            permutation2[i] = Integer.parseInt(line[i]);
        }
        permutation2 = prevPermutation(permutation2);
        if (permutation2 == null) {
            for (int i = 0; i < n; i++) {
                writer.print(0 + " ");
            }
        } else {
            for (int i = 0; i < n; i++) {
                writer.print(permutation2[i] + " ");
            }
        }
        writer.print("\n");
        permutation1 = nextPermutation(permutation1);
        if (permutation1 == null) {
            for (int i = 0; i < n; i++) {
                writer.print(0 + " ");
            }
        } else {
            for (int i = 0; i < n; i++) {
                writer.print(permutation1[i] + " ");
            }
        }
        writer.close();
    }

    private static int[] prevPermutation(int[] permutation) {
        final int size = permutation.length;
        for (int i = size - 2; i >= 0; i--) {
            if (permutation[i] > permutation[i + 1]) {
                int maxIndex = i + 1;
                for (int j = i + 1; j < size; j++) {
                    if (permutation[j] > permutation[maxIndex] && permutation[j] < permutation[i]) {
                        maxIndex = j;
                    }
                }
                swap(permutation, i, maxIndex);
                reverse(permutation, i + 1, size - 1);
                return permutation;
            }
        }
        return new int[size];
    }

    private static int[] nextPermutation(int[] permutation) {
        int min;
        for (int i = n - 2; i > -1; i--) {
            if (permutation[i] < permutation[i + 1]) {
                min = i + 1;
                for (int j = i + 1; j < n; j++) {
                    if (permutation[j] < permutation[min] && permutation[j] > permutation[i]) {
                        min = j;
                    }
                }
                swap(permutation, i, min);
                reverse(permutation, i + 1, n - 1);
                return permutation;
            }
        }
        return null;
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
