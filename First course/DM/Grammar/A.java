import java.io.*;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 07.06.2020
 * @version -
 */

public class A {

    public static void main(String[] args) throws IOException {
        FastReader scanner = new FastReader("automaton.in");
        PrintWriter writer = new PrintWriter("automaton.out");
        Grammar grammar = new Grammar();

        int n = scanner.nextInt();
        grammar.StartSymbol = scanner.next().charAt(0);
        for (int i = 0; i < n; i++) {
            char tmp1 = scanner.next().charAt(0);
            grammar.NonTerminalStates.add(tmp1);
            scanner.next();
            String tmp2 = scanner.next();
            grammar.addRules(tmp1, tmp2);

        }


        int m = scanner.nextInt();
        for (int i = 0; i < m; i++) {
            String word = scanner.next();
            if (check(grammar.StartSymbol, 0, word, grammar)) {
                writer.println("yes");
            } else {
                writer.println("no");
            }
        }

        scanner.close();
        writer.close();
    }

    static boolean check(char Current, int pos, String word, Grammar grammar) {
        if (pos == word.length()) {
            return Character.isLowerCase(Current);
        }

        if (grammar.Rules.containsKey(Current)) {
            for (String rule : grammar.Rules.get(Current)) {
                if (rule.charAt(0) != word.charAt(pos)) {
                    continue;
                }
                if (check(rule.length() == 1 ? rule.charAt(0) : rule.charAt(1), pos + 1, word, grammar)) {
                    return true;
                }
            }
        }

        return false;
    }

    public static class Grammar {
        Character StartSymbol;
        HashSet<Character> Alphabet = new HashSet<>();
        HashSet<Character> TerminalStates = new HashSet<>();
        HashSet<Character> NonTerminalStates = new HashSet<>();
        TreeMap<Character, TreeSet<String>> Rules = new TreeMap<>();

        void addRules(char Key, String Value) {
            if (!Rules.containsKey(Key)) {
                Rules.put(Key, new TreeSet<String>());
            }
            Rules.get(Key).add(Value);
        }
    }

    static class FastReader {
        BufferedReader br;
        StringTokenizer st;

        FastReader(String name) throws FileNotFoundException {
            br = new BufferedReader(new
                    InputStreamReader(new
                    FileInputStream(name)));
        }

        FastReader(InputStream input) {
            br = new BufferedReader(new InputStreamReader(input));
        }

        String next() {
            while (st == null || !st.hasMoreElements()) {
                try {
                    st = new StringTokenizer(br.readLine());
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
            return st.nextToken();
        }

        int nextInt() {
            return Integer.parseInt(next());
        }

        void close() throws IOException {
            br.close();
        }
    }
}


