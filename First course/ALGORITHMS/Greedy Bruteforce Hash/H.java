import java.io.*;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 06.06.2020
 * @version -
 */
public class H {
    static ArrayList<ArrayList<Integer>> arr = new ArrayList<>();

    public static void main(String[] args) throws IOException {
        FastReader scanner = new FastReader("vectors2.in");
        PrintWriter writer = new PrintWriter("vectors2.out");
        int n = scanner.nextInt();

        gen(new ArrayList<Integer>(), n, 0, 0);

        writer.println(arr.size());

        for (int i = 0; i < arr.size(); i++) {
            for (int j = 0; j < n; j++) {
                writer.print(arr.get(i).get(j));
            }
            writer.println();
        }

        scanner.close();
        writer.close();
    }

    static void gen(ArrayList<Integer> vector, int n, int level, int count) {
        if (level == n) {
            arr.add(new ArrayList<Integer>());
            for (int i = 0; i < n; i++) {
                arr.get(arr.size() - 1).add(vector.get(i));
            }
        } else {
            vector.add(0);
            gen(vector, n, level + 1, 0);
            vector.remove(vector.size() - 1);
            if (count != 1 || level == 0) {
                vector.add(1);
                gen(vector, n, level + 1, count + 1);
                vector.remove(vector.size() - 1);
            }
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
