import java.io.*;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 11.06.2020
 * @version -
 */

public class B {

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(new File("epsilon.in"));
        PrintWriter writer = new PrintWriter("epsilon.out");
        Grammar grammar = new Grammar();

        String Line = scanner.nextLine();
        int n = 0;
        for (int i = 0; i < Line.length(); i++) {
            if (Character.isDigit(Line.charAt(i))) {
                continue;
            }
            n = Integer.parseInt(Line.substring(0, i));
            grammar.StartSymbol = Line.charAt(i + 1);
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
        }

        TreeSet<Character> Answer = solution(n, grammar);
        for (char Symbol: Answer) {
            writer.print(Symbol + " ");
        }

        scanner.close();
        writer.close();
    }

    static TreeSet<Character> solution(int n, Grammar grammar) {
        TreeSet<Character> Answer = new TreeSet<>();
        boolean flag = true;

        while (flag) {
            flag = false;
            for (char LeftPartOfRules: grammar.Rules.keySet()) {
                for (String Rule: grammar.Rules.get(LeftPartOfRules)) {
                    if (Rule.equals(" ")) {
                        Answer.add(LeftPartOfRules);
                        flag = true;
                    } else {
                        int k = 0;
                        for (int i = 0; i < Rule.length(); i++) {
                            if (Answer.contains(Rule.charAt(i))) {
                                k++;
                            } else {
                                break;
                            }
                        }
                        if (k == Rule.length()) {
                            Answer.add(LeftPartOfRules);
                            flag = true;
                        }
                    }
                }
            }
            for (char ch: Answer) {
                grammar.Rules.remove(ch);
            }
        }

        return Answer;
    }

    public static class Grammar {
        Character StartSymbol;
        HashSet<Character> Alphabet = new HashSet<>();
        HashSet<Character> TerminalStates = new HashSet<>();
        TreeSet<Character> NonTerminalStates = new TreeSet<>();
        TreeMap<Character, TreeSet<String>> Rules = new TreeMap<>();

        void addRules(char Key, String Value) {
            if (!Rules.containsKey(Key)) {
                Rules.put(Key, new TreeSet<String>());
            }
            Rules.get(Key).add(Value);
        }
    }
}


