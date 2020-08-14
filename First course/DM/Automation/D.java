import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Scanner;
import java.util.TreeMap;

/**
 * @author Michale Gerasimov
 * start: 11.05.2020
 * @version -
 */
public class D {
    public static DFA_Structure DFA_Start;

    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("problem4.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("problem4.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static void doDFA (int n, int m, int k) {
        DFA_Start = new DFA_Structure(n);
        for (int i = 0; i < k; i++) {
            DFA_Start.FinalStates.add(scanner.nextInt());
        }
        for (int i = 0; i < m; i++) {
            int tmp1 = scanner.nextInt(), tmp2 = scanner.nextInt();
            char ch = scanner.next().charAt(0);
            DFA_Start.alphabet.add(ch);
            DFA_Start.transfers.get(tmp1).put(ch, tmp2);
        }
    }

    public static void main(String[] args) {
        int n = scanner.nextInt(), m = scanner.nextInt() , k = scanner.nextInt(), length = scanner.nextInt();
        doDFA(n, m, k);

        int[][] dp = new int[length + 1][DFA_Start.transfers.size()];

        for (int i = 0; i < DFA_Start.transfers.size(); i++) {
            if (DFA_Start.FinalStates.contains(i + 1)) {
                dp[0][i] = 1;
            }
            else {
                dp[0][i] = 0;
            }
        }

        for (int i = 0; i < length; i++) {
            for (int j = 1; j < DFA_Start.transfers.size(); j++) {
                for (char ch = 'a'; ch <= 'z'; ch++) {
                    if (DFA_Start.transfers.get(j).get(ch) != 0) {
                        dp[i + 1][j - 1] = (dp[i + 1][j - 1] + dp[i][DFA_Start.transfers.get(j).get(ch) - 1]) % 1000000007;
                    }
                }
            }
        }

        scanner.close();
        writer.print(dp[length][0]);
        for (int i = 0; i < length + 1; i++) {
            for (int j = 0; j < 4; j++) {
                System.out.print(dp[i][j] + " ");
            }
            System.out.println();
        }
        writer.close();
    }

    public static class DFA_Structure {
        public HashSet<Integer> States = new HashSet<>();
        public Integer InitialState = 1;
        public HashSet<Integer> FinalStates = new HashSet<>();
        public HashSet<Character> alphabet = new HashSet<>();
        public ArrayList<TreeMap<Character, Integer>> transfers = new ArrayList<>();

        public DFA_Structure (int n) {
            for (int i = 0; i < n + 1; i++) {
                this.States.add(i);
                this.transfers.add(new TreeMap<>());
                for (char ch = 'a'; ch <= 'z'; ch++) {
                    this.transfers.get(i).put(ch, 0);
                }
            }

        }
    }
}