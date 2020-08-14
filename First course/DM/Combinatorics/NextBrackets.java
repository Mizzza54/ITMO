import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Scanner;

/**
 * @author Michale Gerasimov
 * start: 17.12.2019
 * @version -
 */
public class NextBrackets {

    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("nextbrackets.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("nextbrackets.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        String sequence;
        sequence = scanner.nextLine();
        sequence = nextBrackets(sequence);
        writer.print(sequence);
        writer.close();
    }

    public static String nextBrackets(String s) {
        int counter_close = 0, counter_open = 0;
        for (int i = s.length() - 1; i > -1; i--) {
            if (s.charAt(i) == '(') {
                counter_open++;
                if (counter_close > counter_open) {
                    break;
                }
            } else {
                counter_close++;
            }
        }
        StringBuilder tmp = new StringBuilder(s);
        tmp.delete(s.length() - counter_close - counter_open, s.length());
        String s1 = tmp.toString();
        if (s1.equals("")) {
            return "-";
        } else {
            tmp.append(")");
            for (int j = 1; j < counter_open + 1; j++) {
                tmp.append("(");
            }
            for (int j = 1; j < counter_close; j++) {
                tmp.append(")");
            }
            return tmp.toString();
        }
    }
}
