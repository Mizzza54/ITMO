import java.io.*;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 04.06.2020
 * @version -
 */
public class A {

    public static void main(String[] args) throws IOException {
        FastReader scanner = new FastReader("cobbler.in");
        PrintWriter writer = new PrintWriter("cobbler.out");
        int k = scanner.nextInt();
        int n = scanner.nextInt();
        ArrayList<Integer> array = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            array.add(scanner.nextInt());
        }
        array.sort(Integer::compareTo);

        int result = 0;
        for (int i = 0; i < n; i++) {
            if (k >= array.get(i)) {
                k -= array.get(i);
                result++;
            } else {
                break;
            }
        }

        writer.println(result);
        scanner.close();
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
