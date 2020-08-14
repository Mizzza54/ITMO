import java.io.*;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 24.02.2020
 * @version -
 */
public class K {
    final static int N = 1 << 17;
    public static int[] t;
    public static int n, m;

    public static void main(String[] args) throws FileNotFoundException {
        FastReader scanner = new FastReader("parking.in");
        PrintWriter writer = new PrintWriter("parking.out");

        n = scanner.nextInt();
        m = scanner.nextInt();

        TreeSet<Integer> tree = new TreeSet<>();

        for (int i = 0; i < n; i++) {
            tree.add(i + 1);
        }

        String str;
        int x;
        for (int i = 0; i < m; i++) {
            str = scanner.next();
            x = scanner.nextInt();
            if (str.charAt(1) == 'n') { // enter
                if (tree.contains(x)) {
                    writer.println(x);
                    tree.remove(x);
                } else if (tree.size() == 1) {
                    writer.println(tree.last());
                    tree.clear();
                } else if (tree.higher(x) != null) {
                    writer.println(tree.higher(x));
                    tree.remove(tree.higher(x));
                } else {
                    writer.println(tree.first());
                    tree.remove(tree.first());
                }
            } else if (str.charAt(1) == 'x') { // exit
                tree.add(x);
            }
        }
        writer.close();
    }

    static class FastReader {
        BufferedReader br;
        StringTokenizer st;

        FastReader(String name) throws FileNotFoundException {
            br = new BufferedReader(new
                    InputStreamReader(new
                    FileInputStream(name)));
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
    }
}
