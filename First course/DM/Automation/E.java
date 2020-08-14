import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 12.05.2020
 * @version -
 */
public class E {
    public static HashSet<Integer> Terminal = new HashSet<>();
    public static ArrayList<TreeMap<Character, HashSet<Integer>>> Transfers = new ArrayList<>();
    public static ArrayDeque<HashSet<Integer>> P = new ArrayDeque<>();
    public static ArrayList<HashSet<Integer>> StatesOfDFA = new ArrayList<>();
    public static ArrayList<TreeMap<Character, Integer>> TransfersOfDFA = new ArrayList<>();
    public static HashSet<Integer> TerminalStatesOfDFA = new HashSet<>();

    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("problem5.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("problem5.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static void getDFAbyNFA() {
        HashSet<Integer> tmp = new HashSet<>();
        tmp.add(1);
        P.add(tmp);
        StatesOfDFA.add(tmp);

        while (!P.isEmpty()) {
            HashSet<Integer> State = new HashSet<>(P.pop());
            for (char ch = 'a'; ch <= 'z'; ch++) {
                HashSet<Integer> nextState = new HashSet<>();

                for (int cur: State) {
                    //qd = qd.add(δ(p,ch));
                    if (Transfers.size() > cur && Transfers.get(cur).get(ch) != null) {
                        nextState.addAll(Transfers.get(cur).get(ch));
                    }
                }

                if (!StatesOfDFA.contains(nextState) && !nextState.isEmpty()) {
                    P.push(nextState);
                    StatesOfDFA.add(nextState);
                }

                if (StatesOfDFA.contains(nextState)) {
                    TransfersOfDFA.get(StatesOfDFA.indexOf(State) + 1).put(ch, StatesOfDFA.indexOf(nextState) + 1);
                }
            }
        }

        for (int i = 0; i < StatesOfDFA.size(); i++) {
            for (int set: StatesOfDFA.get(i)) {
                if (Terminal.contains(set)) {
                    TerminalStatesOfDFA.add(i + 1);
                    break;
                }
            }
        }
    }

    public static void main(String[] args) {
        int n = scanner.nextInt(), m = scanner.nextInt() , k = scanner.nextInt(), length = scanner.nextInt();

        for (int i = 0; i < k; i++) {
            Terminal.add(scanner.nextInt());
        }

        for (int i = 0; i < n + 1; i++) {
            Transfers.add(new TreeMap<>());
        }
        int a, b;
        for (int i = 0; i < m; i++) {
            char ch;
            a = scanner.nextInt();
            b = scanner.nextInt();
            ch = scanner.next().charAt(0);
            if (!Transfers.get(a).containsKey(ch)) {
                Transfers.get(a).put(ch, new HashSet<>());
            }
            Transfers.get(a).get(ch).add(b);
        }

        //System.out.println(Transfers);


        for (int i = 0; i < 1000; i++) {
            TransfersOfDFA.add(new TreeMap<>());
            for (char ch = 'a'; ch <= 'z'; ch++) {
                TransfersOfDFA.get(i).put(ch, 0);
            }
        }

        getDFAbyNFA();

        System.out.println(StatesOfDFA);
        System.out.println(TerminalStatesOfDFA);
        System.out.println(TransfersOfDFA.get(0));
        System.out.println(TransfersOfDFA.get(1));
        System.out.println(TransfersOfDFA.get(2));
        System.out.println(TransfersOfDFA.get(3));



        int[][] dp = new int[length + 1][TransfersOfDFA.size()];

        for (int i = 0; i < TransfersOfDFA.size(); i++) {
            if (TerminalStatesOfDFA.contains(i + 1)) {
                dp[0][i] = 1;
            }
            else {
                dp[0][i] = 0;
            }
        }

        for (int i = 0; i < length; i++) {
            for (int j = 1; j < TransfersOfDFA.size(); j++) {
                for (char ch = 'a'; ch <= 'z'; ch++) {
                    if (TransfersOfDFA.get(j).get(ch) != 0) {
                        dp[i + 1][j - 1] = (dp[i + 1][j - 1] + dp[i][TransfersOfDFA.get(j).get(ch) - 1]) % 1000000007;
                    }
                }
            }
        }


        for (int i = 0; i < length + 1; i++) {
            for (int j = 0; j < 10; j++) {
                System.out.print(dp[i][j] + " ");
            }
            System.out.println();
        }



        scanner.close();
        writer.print(dp[length][0]);
        writer.close();
    }
}