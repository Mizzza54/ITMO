import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Scanner;

/**
 * @author Michale Gerasimov
 * start: 08.12.2019
 * @version -
 */
public class Brackets {
    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("brackets.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("brackets.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        int n = scanner.nextInt();
        generate(n, 0, 0, "");
        writer.close();
    }

    public static void generate(int n, int count_open, int count_close, String ans) {
        if (count_open + count_close == 2 * n) {
            writer.println(ans);
            return;
        }
        if (count_open < n) {
            generate(n, count_open + 1, count_close, ans + "(");
        }
        if (count_open > count_close) {
            generate(n, count_open, count_close + 1, ans + ")");
        }
    }
}