import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Scanner;
import java.util.TreeMap;

/**
 * @author Michale Gerasimov
 * start: 02.05.2020
 * @version -
 */
public class A {
    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("problem1.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("problem1.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }


    public static void main(String[] args) {

        String word = scanner.next();
        int n = scanner.nextInt(), m = scanner.nextInt() , k = scanner.nextInt();
        Struct[] array = new Struct[n + 1];
        for (int i = 0; i < n + 1; i++) {
            array[i] = new Struct();
        }

        for (int i = 0; i < k; i++) {
            int tmp = scanner.nextInt();
            array[tmp].terminal = true;
        }

        for (int i = 0; i < m; i++) {
            int tmp1 = scanner.nextInt(), tmp2 = scanner.nextInt();
            char ch = scanner.next().charAt(0);
            array[tmp1].transfers.put(ch, tmp2);
        }

        int current = 1;
        for (int i = 0; i < word.length(); i++) {
            if (!array[current].transfers.containsKey(word.charAt(i))) {
                writer.println("Rejects");
                writer.close();
                System.exit(0);
            }
            current = array[current].transfers.get(word.charAt(i));
        }

        if (array[current].terminal) {
            writer.println("Accepts");
        } else {
            writer.println("Rejects");
        }
        writer.close();
    }


    public static class Struct {
        public boolean terminal = false;
        public TreeMap<Character, Integer> transfers = new TreeMap<>();
    }
}
