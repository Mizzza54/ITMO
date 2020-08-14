import java.io.*;
import java.util.Arrays;
import java.util.Comparator;
import java.util.StringTokenizer;

/**
 * @author Michale Gerasimov
 * start: 07.06.2020
 * @version -
 */

public class F {
    static long ans = 0;

    public static Card[] merge(Card[] a, Card[] b) {
        int i = 0;
        int j = 0;
        int n = a.length;
        int m = b.length;
        Card[] c = new Card[a.length + b.length];
        while ((i < n) || (j < m)) {

            if ((j == m || ((i < n) && a[i].blue <= b[j].blue))) {
                c[i+j] = new Card(a[i].red, a[i].blue);
                i++;
            } else {
                ans = ans + (n - i);
                c[i+j] = new Card(b[j].red, b[j].blue);
                j++;
            }
        }
        return c;
    }

    public static Card[] mergesort(Card[] a) {
        int n = a.length;
        if (a.length <= 1) {
            return a;
        } else {
            Card[] l = new Card[n/2];
            Card[] r = new Card[n - n/2];
            for (int i = 0; i < n; i++) {
                if (i < n/2) {
                    l[i] = new Card(a[i].red, a[i].blue);
                } else {
                    r[i - (n / 2)] = new Card(a[i].red, a[i].blue);
                }
            }
            l = mergesort(l);
            r = mergesort(r);
            return merge(l, r);
        }
    }

    static Comparator<Card> myCMP = new Comparator<Card>() {
        @Override
        public int compare(Card o1, Card o2) {
            if (o1.red - o2.red > 0) {
                return 2;
            } else if (o1.red - o2.red < 0){
                return -2;
            } else {
                if (o1.blue - o2.blue > 0) {
                    return 2;
                } else if (o1.blue - o2.blue == 0) {
                    return 0;
                } else {
                    return -2;
                }
            }
        }

        @Override
        public boolean equals(Object obj) {
            return false;
        }
    };

    public static void main(String[] args) throws IOException {
        FastReader scanner = new FastReader("john.in");
        PrintWriter writer = new PrintWriter("john.out");
        int n = (int) scanner.nextInt();
        Card[] array = new Card[n];

        for (int i = 0; i < n; i++) {
            array[i] = new Card(scanner.nextInt(), scanner.nextInt());
        }



        Arrays.sort(array, myCMP);

        mergesort(array);

        /*
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (i < j && array[i].blue > array[j].blue) {
                    ans++;
                }
            }
        }
         */

        writer.println(ans);
        scanner.close();
        writer.close();
    }

    static class Card {
        long red, blue;

        Card(long red, long blue) {
            this.red = red;
            this.blue = blue;
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

        long nextInt() {
            return Long.parseLong(next());
        }

        void close() throws IOException {
            br.close();
        }
    }
}


