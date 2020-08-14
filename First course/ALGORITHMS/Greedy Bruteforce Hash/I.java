import java.io.*;
import java.util.ArrayList;
import java.util.StringTokenizer;

/**
 * @author Michale Gerasimov
 * start: 07.06.2020
 * @version -
 */

public class I {
    static ArrayList<ArrayList<Integer>> arr = new ArrayList<>();

    public static void main(String[] args) throws IOException {
        FastReader scanner = new FastReader("sqroot.in");
        PrintWriter writer = new PrintWriter("sqroot.out");
        int[][] Matrix = new int[4][4];

        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 4; j++) {
                Matrix[i][j] = scanner.nextInt();
            }
        }

        gen(new ArrayList<Integer>(), 0);
        int[][] SQRT = new int[4][4];

        for (int m = 0; m < 65536; m++) {
            int k = 0;
            for (int i = 0; i < 4; i++) {
                for (int j = 0; j < 4; j++) {
                    SQRT[i][j] = arr.get(m).get(k);
                    k++;
                }
            }


            if (MatrixEquals(mul(SQRT, SQRT), Matrix)) {
                for (int i = 0; i < 4; i++) {
                    for (int j = 0; j < 4; j++) {
                        writer.print(SQRT[i][j] + " ");
                    }
                    writer.println();
                }

                scanner.close();
                writer.close();
                System.exit(0);
            }
        }

        writer.println("NO SOLUTION");
        scanner.close();
        writer.close();
    }

    static void gen(ArrayList<Integer> vector, int level) {
        if (level == 16) {
            arr.add(new ArrayList<Integer>());
            for (int i = 0; i < 16; i++) {
                arr.get(arr.size() - 1).add(vector.get(i));
            }
        } else {
            vector.add(0);
            gen(vector, level + 1);
            vector.remove(vector.size() - 1);
            vector.add(1);
            gen(vector, level + 1);
            vector.remove(vector.size() - 1);
        }
    }

    static boolean MatrixEquals(int[][] A, int[][] B) {
        boolean res = true;
        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 4; j++) {
                if (A[i][j] != B[i][j]) {
                    res = false;
                    break;
                }
            }
            if (!res) {
                break;
            }
        }
        return res;
    }

    static int[][] mul(int[][] A, int[][] B) {
        int[][] res = new int[4][4];
        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 4; j++) {
                for (int k = 0; k < 4; k++) {
                    res[i][j] += A[i][k] * B[k][j];
                }
                res[i][j] = res[i][j] % 2;
            }
        }
        return res;
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


