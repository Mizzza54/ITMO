import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Scanner;
import java.util.TreeMap;

/**
 * @author Michale Gerasimov
 * start: 04.05.2020
 * @version -
 */
public class G {
    public static ArrayList<Character> alphabet = new ArrayList<>();

    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("equivalence.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("equivalence.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static Struct[] doDFA (int n, int m, int k) {
        Struct[] DFA = new Struct[n + 1];
        for (int i = 0; i < n + 1; i++) {
            DFA[i] = new Struct();
        }

        for (int i = 0; i < k; i++) {
            DFA[scanner.nextInt()].terminal = true;
        }

        for (int i = 0; i < m; i++) {
            int tmp1 = scanner.nextInt(), tmp2 = scanner.nextInt();
            char ch = scanner.next().charAt(0);
            if (!alphabet.contains(ch)) {
                alphabet.add(ch);
            }
            DFA[tmp1].transfers.put(ch, tmp2);
        }

        return DFA;
    }

    public static boolean EquivalenceCheck (Struct[] DFA1, Struct[] DFA2) {
        ArrayDeque<Pair> queue = new ArrayDeque<>();
        boolean[][] used = new boolean[DFA1.length][DFA2.length];
        queue.push(new Pair(1, 1));
        Pair pair;
        while (!queue.isEmpty()) {
            pair = queue.pop();
            if (DFA1[pair.first].terminal != DFA2[pair.second].terminal) {
                return false;
            }
            used[pair.first][pair.second] = true;
            for (char ch : alphabet) {
                if (DFA1[pair.first].transfers.containsKey(ch) && DFA2[pair.second].transfers.containsKey(ch)) {
                    if (!used[DFA1[pair.first].transfers.get(ch)][DFA2[pair.second].transfers.get(ch)]) {
                        queue.push(new Pair(DFA1[pair.first].transfers.get(ch), DFA2[pair.second].transfers.get(ch)));
                    }
                } else if (DFA1[pair.first].transfers.containsKey(ch)) {
                    DFA2[pair.second].transfers.put(ch, 0);
                    queue.push(new Pair(DFA1[pair.first].transfers.get(ch), DFA2[pair.second].transfers.get(ch)));
                } else if (DFA2[pair.second].transfers.containsKey(ch)) {
                    DFA1[pair.first].transfers.put(ch, 0);
                    queue.push(new Pair(DFA1[pair.first].transfers.get(ch), DFA2[pair.second].transfers.get(ch)));
                }
            }
        }
        return true;
    }

    public static void main(String[] args) {

        Struct[] DFA1, DFA2;
        int n = scanner.nextInt(), m = scanner.nextInt() , k = scanner.nextInt();
        DFA1 = doDFA(n, m, k);
        n = scanner.nextInt();
        m = scanner.nextInt();
        k = scanner.nextInt();
        DFA2 = doDFA(n, m, k);

        if (EquivalenceCheck(DFA1, DFA2)) {
            writer.println("YES");
        } else {
            writer.println("NO");
        }
        writer.close();
    }


    public static class Pair {
        public int first, second;

        public Pair(int first, int second) {
            this.first = first;
            this.second = second;
        }
    }

    public static class Struct {
        public boolean terminal = false;
        public TreeMap<Character, Integer> transfers = new TreeMap<>();
    }
}
