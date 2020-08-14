
import java.io.*;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 11.06.2020
 * @version -
 */

public class C {

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(new File("useless.in"));
        PrintWriter writer = new PrintWriter("useless.out");
        Grammar grammar = new Grammar();

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
            for (int j = 0; j < tmp2.length(); j++) {
                if (Character.isUpperCase(tmp2.charAt(j))) {
                    grammar.NonTerminalStates.add(tmp2.charAt(j));
                }
            }
        }

        TreeSet<Character> Answer1 = findGenerating(grammar);
        TreeSet<Character> NonGenerating = new TreeSet<>();
        for (char ch: grammar.NonTerminalStates) {
            if (!Answer1.contains(ch)) {
                NonGenerating.add(ch);
            }
        }

        for (char ch: NonGenerating) {
            grammar.Rules.remove(ch);
        }

        ArrayList<Character> DeleteKey = new ArrayList<>();
        ArrayList<String> DeleteRule = new ArrayList<>();
        for (char LeftPartOfRules: grammar.Rules.keySet()) {
            for (String Rule: grammar.Rules.get(LeftPartOfRules)) {
                for (int i = 0; i < Rule.length(); i++) {
                    if (NonGenerating.contains(Rule.charAt(i))) {
                        DeleteKey.add(LeftPartOfRules);
                        DeleteRule.add(Rule);
                        break;
                    }
                }
            }
        }

        for (int i = 0; i < DeleteKey.size(); i++) {
            grammar.Rules.get(DeleteKey.get(i)).remove(DeleteRule.get(i));
        }

        TreeSet<Character> Answer2 = findUnreachable(grammar);
        TreeSet<Character> Unreachable = new TreeSet<>();
        for (char ch: grammar.NonTerminalStates) {
            if (!Answer2.contains(ch)) {
                Unreachable.add(ch);
            }
        }

        TreeSet<Character> Answer = new TreeSet<>();
        Answer.addAll(Unreachable);
        Answer.addAll(NonGenerating);

        for (char ch: Answer) {
            writer.print(ch + " ");
        }

        scanner.close();
        writer.close();
    }

    static TreeSet<Character> findGenerating(Grammar grammar) {
        TreeSet<Character> Answer = new TreeSet<>();
        boolean flag = true;

        while (flag) {
            flag = false;
            for (char LeftPartOfRules: grammar.Rules.keySet()) {
                for (String Rule: grammar.Rules.get(LeftPartOfRules)) {
                    int countOfLower = 0;
                    int countOfUpper = 0;
                    int MainCounter = 0;
                    for (int i = 0; i < Rule.length(); i++) {
                        if (Character.isLowerCase(Rule.charAt(i))) {
                            countOfLower++;
                        }

                        if (Character.isUpperCase(Rule.charAt(i))) {
                            countOfUpper++;
                            if (Answer.contains(Rule.charAt(i))) {
                                MainCounter++;
                            }
                        }
                    }

                    if (countOfLower == Rule.length() && !Answer.contains(LeftPartOfRules)) {
                        flag = true;
                        Answer.add(LeftPartOfRules);
                    }

                    if (countOfUpper == MainCounter && !Answer.contains(LeftPartOfRules)) {
                        flag = true;
                        Answer.add(LeftPartOfRules);
                    }
                }
            }
        }
        return Answer;
    }

    static TreeSet<Character> findUnreachable(Grammar grammar) {
        TreeSet<Character> Answer = new TreeSet<>();
        Answer.add(grammar.StartSymbol);
        boolean flag = true;

        while (flag) {
            flag = false;
            for (char LeftPartOfRules: grammar.Rules.keySet()) {
                if (Answer.contains(LeftPartOfRules)) {
                    for (String Rule: grammar.Rules.get(LeftPartOfRules)) {
                        for (int i = 0; i < Rule.length(); i++) {
                            if (Character.isUpperCase(Rule.charAt(i)) && !Answer.contains(Rule.charAt(i))) {
                                Answer.add(Rule.charAt(i));
                                flag = true;
                            }
                        }
                    }
                }
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


