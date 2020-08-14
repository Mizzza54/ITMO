import java.io.*;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 04.06.2020
 * @version -
 */
public class B {

    public static void main(String[] args) throws IOException {
        FastReader scanner = new FastReader("request.in");
        PrintWriter writer = new PrintWriter("request.out");
        int n = scanner.nextInt();
        ArrayList<Pair> array = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            array.add(new Pair());
            array.get(i).start = scanner.nextInt();
            array.get(i).end = scanner.nextInt();
        }

        Comparator<Pair> myCMP = new Comparator<Pair>() {
            @Override
            public int compare(Pair o1, Pair o2) {
                return o1.end - o2.end;
            }

            @Override
            public boolean equals(Object obj) {
                return false;
            }
        };

        array.sort(myCMP);

        int result = 1;
        int j = 0;
        for (int i = 1; i < n; i++) {
            if (array.get(i).start >= array.get(j).end) {
                result++;
                j = i;
            }
        }

        writer.println(result);
        scanner.close();
        writer.close();
    }

    static class Pair {
        int start, end;
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
