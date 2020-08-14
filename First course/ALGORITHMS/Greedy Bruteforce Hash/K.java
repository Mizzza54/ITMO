import java.io.*;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 08.06.2020
 * @version -
 */
public class K {

    public static void main(String[] args) throws IOException {
        FastReader scanner = new FastReader("dowry.in");
        PrintWriter writer = new PrintWriter("dowry.out");
        int n = scanner.nextInt();
        long L = scanner.nextLong();
        long R = scanner.nextLong();
        Pair[] array = new Pair[n];
        
        for (int i = 0; i < n; i++) {
            array[i] = new Pair(scanner.nextInt(), scanner.nextInt());
        }

        long MaxPrice = 0;
        int MaxCount = 0;
        long MaxMask = 0;

        for (long mask = 0; mask < (1 << n); mask++) {
            long price = 0;
            long weight = 0;
            int count = 0;
            for (int i = 0; i < n; i++) {
                long tmp = mask & (1 << i);

                if (tmp > 0) {
                    price += array[i].price;
                    weight += array[i].weight;
                    count++;
                }

                if (weight > R) {
                    break;
                }
            }
            if (price > MaxPrice && L <= weight && weight <= R) {
                MaxPrice = price;
                MaxCount = count;
                MaxMask = mask;
            }
        }

        if (MaxCount == 0) {
            writer.println(0);
        } else {
            writer.println(MaxCount);

            for (int i = 0; i < n; i++) {
                long tmp = MaxMask & (1 << i);
                if (tmp > 0) {
                    writer.println(i + 1);
                }
            }
        }
        scanner.close();
        writer.close();
    }

    static class Pair {
        int weight, price;
        
        Pair(int weight, int price) {
            this.weight = weight;
            this.price = price;
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
