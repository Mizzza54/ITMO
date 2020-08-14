import java.io.*;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.StringTokenizer;

/**
 * @author Michale Gerasimov
 * start: 05.06.2020
 * @version -
 */

public class E {

    public static void main(String[] args) throws IOException {
        FastReader scanner = new FastReader("apples.in");
        PrintWriter writer = new PrintWriter("apples.out");
        int n = scanner.nextInt();
        int height = scanner.nextInt();

        if (height <= 0) {
            writer.println("-1");
            writer.close();
            System.exit(0);
        }

        ArrayList<Apple> array1 = new ArrayList<>();
        ArrayList<Apple> array2 = new ArrayList<>();
        ArrayList<Apple> array3 = new ArrayList<>();
        for (int i = 0, up, down; i < n; i++) {
            down = scanner.nextInt();
            up = scanner.nextInt();

            if (up > down) {
                array1.add(new Apple(down, up, i + 1));
            } else if (up < down) {
                array2.add(new Apple(down, up, i + 1));
            } else {
                array3.add(new Apple(down, up, i + 1));
            }
        }

        Comparator<Apple> myCMP1 = new Comparator<Apple>() {
            @Override
            public int compare(Apple o1, Apple o2) {
                return o1.down - o2.down;
            }

            @Override
            public boolean equals(Object obj) {
                return false;
            }
        };

        array1.sort(myCMP1);
        array3.sort(myCMP1);

        Comparator<Apple> myCMP2 = new Comparator<Apple>() {
            @Override
            public int compare(Apple o1, Apple o2) {
                return Math.abs((o2.up)) - Math.abs((o1.up));
            }

            @Override
            public boolean equals(Object obj) {
                return false;
            }
        };

        array2.sort(myCMP2);

        ArrayList<Integer> result = new ArrayList<>();
        for (Apple value : array1) {
            if (height - value.down > 0) {
                height = height - value.down + value.up;
                result.add(value.id);
            } else {
                writer.println("-1");
                writer.close();
                System.exit(0);
            }
        }

        if (height <= 0) {
            writer.println("-1");
            writer.close();
            System.exit(0);
        }

        for (Apple value : array3) {
            if (height - value.down > 0) {
                height = height - value.down + value.up;
                result.add(value.id);
            } else {
                writer.println("-1");
                writer.close();
                System.exit(0);
            }
        }

        if (height <= 0) {
            writer.println("-1");
            writer.close();
            System.exit(0);
        }

        for (Apple apple : array2) {
            if (height - apple.down > 0) {
                height = height - apple.down + apple.up;
                result.add(apple.id);
            } else {
                writer.println("-1");
                writer.close();
                System.exit(0);
            }
            // нужен ли этот else?
        }

        if (height <= 0) {
            writer.println("-1");
            writer.close();
            System.exit(0);
        }


        if (result.size() == n) {
            for (Integer integer : result) {
                writer.print(integer + " ");
            }
        } else {
            writer.println("-1");
        }
        scanner.close();
        writer.close();
    }

    static class Apple {
        int up, down, id;

        Apple(int down, int up, int id) {
            this.up = up;
            this.down = down;
            this.id = id;
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


