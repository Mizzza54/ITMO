
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 12.12.2019
 * @version -
 */
public class Subsets {
    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("subsets.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("subsets.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        int n = scanner.nextInt();
        Subsets(n);
        writer.close();
    }

    public static void Subsets(int n) {
        TreeMap<StringBuilder, Boolean> sets = new TreeMap<>();
        int[] setInt;
        StringBuilder set;
        for (int i = 0; i < n; i++) {
            setInt = new int[i + 1];
            for (int j = 0; j < i + 1; j++) {
                setInt[j] = j + 1;
            }
            set = new StringBuilder();
            for (int k = 0; k < setInt.length; k++) {
                if (setInt[k] != 10) {
                    set.append(setInt[k] + " ");
                } else {
                    set.append("9" + setInt[k]);
                }
            }
            sets.put(set, true);
            while ((setInt = nextChoose(setInt, n, i + 1)) != null) {
                set = new StringBuilder();
                for (int k = 0; k < setInt.length; k++) {
                    if (setInt[k] != 10) {
                        set.append(setInt[k] + " ");
                    } else {
                        set.append("9" + setInt[k]);
                    }
                }
                sets.put(set, true);
            }
        }
        writer.println();
        for (StringBuilder ans: sets.keySet()) {
            if (ans.toString().contains("910")) {
                writer.println(ans.substring(0, ans.length() - 3) + "10");
            } else {
                writer.println(ans);
            }
        }
        writer.close();
    }

    public static int[] nextChoose(int[] a, int n, int k) {
        int[] tmp = new int[k + 1];
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