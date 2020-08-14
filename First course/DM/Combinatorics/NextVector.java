import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Scanner;

/**
 * @author Michale Gerasimov
 * start: 16.12.2019
 * @version -
 */
public class NextVector {
    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("nextvector.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("nextvector.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        String line = scanner.nextLine();
        int n = line.length();
        int[] vector1 = new int[n];
        int[] vector2 = new int[n];
        for (int i = 0; i < n; i++) {
            vector1[i] = Integer.parseInt(String.valueOf(line.charAt(i)));
            vector2[i] = Integer.parseInt(String.valueOf(line.charAt(i)));
        }
        int[] tmp;
        tmp  = prevVector(vector1, n);
        if (tmp != null) {
            for (int bit : tmp) {
                writer.print(bit);
            }
        } else {
            writer.print("-");
        }
        writer.print("\n");
        tmp = nextVector(vector2, n);
        if (tmp != null) {
            for (int bit : tmp) {
                writer.print(bit);
            }
        } else {
            writer.print("-");
        }
        writer.close();
    }

    public static int[] nextVector(int[] vector, int n) {
        n--;
        while (n >= 0 && vector[n] != 0) {
            vector[n]= 0;
            n--;
        }
        if (n == -1) {
            return null;
        }
        vector[n] = 1;
        return vector;
    }

    public static int[] prevVector(int[] vector, int n) {
        n--;
        while (n >= 0 && vector[n] != 1) {
            vector[n]= 1;
            n--;
        }
        if (n == -1) {
            return null;
        }
        vector[n] = 0;
        return vector;
    }
}
