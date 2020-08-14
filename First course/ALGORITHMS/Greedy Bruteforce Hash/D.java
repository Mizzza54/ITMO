import java.io.*;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 04.06.2020
 * @version -
 */
public class D {

    public static void main(String[] args) throws IOException {
        FastReader scanner = new FastReader("sequence.in");
        PrintWriter writer = new PrintWriter("sequence.out");
        int n = scanner.nextInt();
        int sum = 0;
        ArrayList<Pair> array = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            int value = scanner.nextInt();
            array.add(new Pair(i + 1, value));
            sum += value;
        }

        Comparator<Pair> myCMP = new Comparator<Pair>() {
            @Override
            public int compare(Pair o1, Pair o2) {
                return o2.value - o1.value;
            }

            @Override
            public boolean equals(Object obj) {
                return false;
            }
        };

        array.sort(myCMP);

        if (sum % 2 != 0) {
            writer.println(-1);
        } else {
            int half1 = sum / 2;
            int half2 = sum / 2;
            int count = 0;
            ArrayList<Integer> result = new ArrayList<>();

            for (int i = 0; i < n; i++) {
                if (array.get(i).value <= half1) {
                    half1 -= array.get(i).value;
                    count++;
                    result.add(array.get(i).id);
                } else {
                    half2 -= array.get(i).value;
                }
            }

            if (half1 == 0 && half2 == 0) {
                writer.println(count);
                result.sort(Integer::compareTo);
                for (int i = 0; i < result.size(); i++) {
                    writer.print(result.get(i) + " ");
                }
            } else {
                writer.println(-1);
            }
        }

        scanner.close();
        writer.close();
    }

    static class Pair {
        Integer id, value;

        Pair() {
            this.id = null;
            this.value = null;
        }

        Pair(int id, int value) {
            this.id = id;
            this.value = value;
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
