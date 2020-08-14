import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Scanner;
import java.util.TreeMap;
import java.util.HashSet;

/**
 * @author Michale Gerasimov
 * start: 02.05.2020
 * @version -
 */
public class B {
    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("problem2.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("problem2.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }


    public static void main(String[] args) {

        HashSet<Integer> term = new HashSet<>();
        String word = scanner.next();
        int n = scanner.nextInt(), m = scanner.nextInt() , k = scanner.nextInt();
        Struct[] array = new Struct[n + 1];
        for (int i = 0; i < n + 1; i++) {
            array[i] = new Struct();
        }

        for (int i = 0; i < k; i++) {
            int tmp = scanner.nextInt();
            term.add(tmp);
            array[tmp].terminal = true;
        }

        for (int i = 0; i < m; i++) {
            int tmp1 = scanner.nextInt(), tmp2 = scanner.nextInt();
            char ch = scanner.next().charAt(0);

            if (!array[tmp1].transfers.containsKey(ch)) {
                array[tmp1].transfers.put(ch, new HashSet<>());
            }
            array[tmp1].transfers.get(ch).add(tmp2);
        }

        for (int i = 0; i < array.length; i++) {
            System.out.println(array[i].transfers);
        }

        ArrayList<HashSet<Integer>> current = new ArrayList<>();
        current.add(new HashSet<>());
        current.get(0).add(1);

        for (int i = 1; i < word.length() + 1; i++) {
            current.add(new HashSet<>());
            for (Integer j: current.get(i - 1)) {
                if (array[j].transfers.containsKey(word.charAt(i - 1))) {
                    current.get(i).addAll(array[j].transfers.get(word.charAt(i - 1)));
                }
            }
        }

        System.out.println(current.get(word.length()));
        if (current.get(word.length()).containsAll(term)) {
            writer.println("Accepts");
        } else {
            writer.println("Rejects");
        }
        writer.close();
    }


    public static class Struct {
        public boolean terminal = false;
        public TreeMap<Character, HashSet<Integer>> transfers = new TreeMap<>();
    }
}
