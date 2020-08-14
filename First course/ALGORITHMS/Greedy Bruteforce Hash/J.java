import java.io.*;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 15.06.2020
 * @version -
 */
public class J {

    public static void main(String[] args) throws IOException {
        FastReader scanner = new FastReader("jurassic.in");
        PrintWriter writer = new PrintWriter("jurassic.out");
        int n = scanner.nextInt();
        Pair[] array = new Pair[n];

        for (int i = 0; i < n; i++) {
            array[i] = new Pair(scanner.next(), i + 1);
        }

        int ResultSize = 0;
        int ResultMask = 0;

        for (int mask = 0; mask < (1 << n); mask++) {

            int j = mask;
            int CurrentMask = 0;
            int SizeCurrentMask = 0;

            for (int i = 0; j > 0; i++, j >>= 1) {
                int tmp = j & 1;
                if (tmp != 0) {
                    CurrentMask ^= array[i].mask;
                    SizeCurrentMask++;
                }
            }

            if (CurrentMask == 0 && SizeCurrentMask > ResultSize) {
                ResultMask = mask;
                ResultSize = SizeCurrentMask;
            }

        }

        writer.println(ResultSize);
        for (int i = 1; ResultMask > 0; i++, ResultMask >>= 1) {
            int tmp = ResultMask & 1;
            if (tmp != 0) {
                writer.print(i + " ");
            }
        }

        scanner.close();
        writer.close();
    }

    static class Pair {
        ArrayList<Character> letters;
        int id;
        int mask;

        Pair(String letters, int id) {
            this.id = id;
            this.mask = 0;
            this.letters = new ArrayList<>();
            for (char ch: letters.toCharArray()) {
                this.letters.add(ch);
                mask += 1 << (ch - 'A');
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

        long nextLong() {
            return Long.parseLong(next());
        }

        void close() throws IOException {
            br.close();
        }
    }
}
