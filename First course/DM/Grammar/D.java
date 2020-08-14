
import java.io.*;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 11.06.2020
 * @version -
 */

public class D {

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(new File("nfc.in"));
        PrintWriter writer = new PrintWriter("nfc.out");
        Grammar grammar = new Grammar();
        long MOD = 1000000007;


        String Line = scanner.nextLine();
        int n = 0;
        for (int i = 0; i < Line.length(); i++) {
            if (Character.isDigit(Line.charAt(i))) {
                continue;
            }
            n = Integer.parseInt(Line.substring(0, i));
            grammar.StartSymbol = Line.charAt(i + 1);
            grammar.NonTerminalStates.add(grammar.StartSymbol);
            break;
        }

        for (int i = 0; i < n; i++) {
            Line = scanner.nextLine();
            char tmp1 = Line.charAt(0);
            grammar.NonTerminalStates.add(tmp1);
            String tmp2;
            if (Line.length() == 4 || Line.length() == 5) {
                tmp2 = " ";
            } else {
                tmp2 = Line.substring(5, Line.length());
            }
            grammar.addRules(tmp1, tmp2);
            if (tmp2.length() == 2) {
                grammar.addRulesABC(tmp1, tmp2);
            }
            for (int j = 0; j < tmp2.length(); j++) {
                if (Character.isUpperCase(tmp2.charAt(j))) {
                    grammar.NonTerminalStates.add(tmp2.charAt(j));
                }
            }
        }

        String word = scanner.nextLine();

        long[][][] dp = new long[26][word.length()][word.length()];

        for (int i = 0; i < word.length(); i++) {
            for (char LeftPartOfRules: grammar.Rules.keySet()) {
                for (String Rule : grammar.Rules.get(LeftPartOfRules)) {
                    if (Rule.length() == 1 && Rule.charAt(0) == word.charAt(i)) {
                        dp[LeftPartOfRules - 65][i][i] = 1;
                        break;
                    }
                }
            }
        }

        for (int i = 0; i < word.length(); i++) {
            for (int j = 0; j < word.length(); j++) {
                if (i + j < word.length()) {
                    for (int k = j; k < i + j; k++) {
                        for (char LeftPartOfRules: grammar.RulesABC.keySet()) {
                            for (String Rule : grammar.RulesABC.get(LeftPartOfRules)) {
                                char A = LeftPartOfRules;
                                char B = Rule.charAt(0);
                                char C = Rule.charAt(1);
                                dp[A - 65][j][i + j] = (dp[A - 65][j][i + j] + dp[B - 65][j][k] * dp[C - 65][k + 1][i + j]) % MOD;
                            }
                        }
                    }
                }
            }
        }

        writer.println(dp[grammar.StartSymbol - 65][0][word.length() - 1]);

        for (int i = 0; i < 5; i++) {
            for (int j = 0; j < word.length(); j++) {
                for (int k = 0; k < word.length(); k++) {
                    System.out.print(dp[i][j][k] + " ");
                }
                System.out.println();
            }
            System.out.println("---------------------------");
        }

        scanner.close();
        writer.close();
    }

    public static class Grammar {
        Character StartSymbol;
        HashSet<Character> Alphabet = new HashSet<>();
        HashSet<Character> TerminalStates = new HashSet<>();
        TreeSet<Character> NonTerminalStates = new TreeSet<>();
        TreeMap<Character, TreeSet<String>> Rules = new TreeMap<>();
        TreeMap<Character, TreeSet<String>> RulesABC = new TreeMap<>();

        void addRules(char Key, String Value) {
            if (!Rules.containsKey(Key)) {
                Rules.put(Key, new TreeSet<String>());
            }
            Rules.get(Key).add(Value);
        }

        void addRulesABC(char Key, String Value) {
            if (!RulesABC.containsKey(Key)) {
                RulesABC.put(Key, new TreeSet<String>());
            }
            RulesABC.get(Key).add(Value);
        }
    }
}


