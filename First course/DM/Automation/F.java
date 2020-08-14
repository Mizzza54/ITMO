import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Scanner;
import java.util.TreeMap;

/**
 * @author Michale Gerasimov
 * start: 05.05.2020
 * @version -
 */
public class F {
    public static boolean[] visited = new boolean[1000000];
    public static int[] associations = new int[1000000];
    public static Struct[] DFA1, DFA2;
    public static ArrayList<Character> alphabet = new ArrayList<>();

    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("fastminimization.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("isomorphism.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static Struct[] doDFA (int n, int m, int k, boolean flag) {
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
            if (!alphabet.contains(ch) && flag) {
                alphabet.add(ch);
            }
            DFA[tmp1].transfers.put(ch, tmp2);
        }

        return DFA;
    }

    public static boolean dfs (int Node1, int Node2) {
        visited[Node1] = true;
        if (DFA1[Node1].terminal != DFA2[Node2].terminal) {
            return false;
        }
        associations[Node1] = Node2;
        boolean result = true;
        for (char ch : alphabet) {
            int t1, t2;
            if (DFA1[Node1].transfers.containsKey(ch)) {
                t1 = DFA1[Node1].transfers.get(ch);
            } else {
                t1 = 0;
            }
            if (DFA2[Node2].transfers.containsKey(ch)) {
                t2 = DFA2[Node2].transfers.get(ch);
            } else {
                t2 = 0;
            }
            if (visited[t1]) {
                result = result && t2 == associations[t1];
            } else {
                result = result && dfs(t1, t2);
            }
        }
        return result;
    }

    public static void main(String[] args) {

        int n = scanner.nextInt(), m = scanner.nextInt() , k = scanner.nextInt();
        DFA1 = doDFA(n, m, k, true);
        n = scanner.nextInt();
        m = scanner.nextInt();
        k = scanner.nextInt();
        DFA2 = doDFA(n, m, k, false);

        if (dfs(1, 1)) {
            writer.println("YES");
        } else {
            writer.println("NO");
        }
        writer.close();
    }

    public static class Struct {
        public boolean terminal = false;
        public TreeMap<Character, Integer> transfers = new TreeMap<>();
    }
}
