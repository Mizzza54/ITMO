import java.io.*;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 04.06.2020
 * @version -
 */
public class C {

    public static void main(String[] args) throws IOException {
        FastReader scanner = new FastReader("printing.in");
        PrintWriter writer = new PrintWriter("printing.out");
        int n = scanner.nextInt();

        if (n == 0) {
            writer.println(0);
            scanner.close();
            writer.close();
            System.exit(0);
        }

        Paper[] array = new Paper[7];
        for (int i = 0, price, pow; i < 7; i++) {
            price = scanner.nextInt();
            pow = (int) Math.pow(10, i);
            array[i] = new Paper(price, pow,  (double) price / (double) pow);
        }

        Comparator<Paper> myCMP = new Comparator<Paper>() {
            @Override
            public int compare(Paper o1, Paper o2) {
                if (o1.PricePerPaper - o2.PricePerPaper > 0) {
                    return 1;
                } else if (o1.PricePerPaper - o2.PricePerPaper == 0) {
                    return 0;
                } else {
                    return -1;
                }
            }

            @Override
            public boolean equals(Object obj) {
                return false;
            }
        };

        Arrays.sort(array, myCMP);

        writer.println(solve(array, 0, n));
        scanner.close();
        writer.close();
    }

    static int solve(Paper[] array, int index, int n) {
        if (n == 0) {
            return 0;
        }

        Paper paper = array[index];
        int minPrice = paper.price * (n / paper.count);
        return minPrice + Math.min(paper.price, solve(array,index + 1,n % paper.count));
    }

    static class Paper {
        int price, count;
        double PricePerPaper;

        Paper(int price, int count, double PricePerPaper) {
            this.price = price;
            this.count = count;
            this.PricePerPaper = PricePerPaper;
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
