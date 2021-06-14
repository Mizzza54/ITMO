import java.io.*;
import java.util.Arrays;

/**
 * @author Michael Gerasimov
 * start: 18.04.2021
 * @version -
 */
public class H {
    StreamTokenizer scanner = new StreamTokenizer(new BufferedReader(new InputStreamReader(System.in)));
    PrintWriter writer = new PrintWriter(System.out);
    Matrix graph;

    public static void main(String[] args) {
        H H = new H();
        H.run();
    }

    public void run() {
        graph = new Matrix();
        hungarianAlgorithm();
        writer.close();
    }

    public void hungarianAlgorithm() {
        int INF = 1_000_000_000;
        /*
         * Массив паросочетания. Для каждого стобца 𝚒=𝟶…𝚖 он хранит номер соответствующей выбранной строки matching[𝚒] (или 𝟶, если ничего не выбрано).
         * Полагаем, что matching[𝟶] равно номеру рассматриваемой строки.
         */
        int[] matching = new int[graph.n + 1];
        /*
         * Массив, хранящий для каждого столбца 𝚓 вспомогательные минимумы, необходимые для быстрого пересчета потенциала.
         */
        int[] minv = new int[graph.n + 1];
        /*
         * Потенциал
         */
        int[] u = new int[graph.n + 1];
        int[] v = new int[graph.n + 1];
        boolean[] used = new boolean[graph.n + 1];
        int[] way = new int[graph.n + 1];

        for (int i = 1; i <= graph.n; i++) {
            matching[0] = i;
            int j0 = 0;
            Arrays.fill(used, false);
            Arrays.fill(minv, INF);
            do {
                used[j0] = true;
                int i0 = matching[j0];
                int delta = INF;
                int j1 = 0;
                for (int j = 1; j <= graph.n; j++) {
                    if (!used[j]) {
                        int cur = graph.matrix[i0][j] - u[i0] - v[j];
                        if (cur < minv[j]) {
                            minv[j] = cur;
                            way[j] = j0;
                        }
                        if (minv[j] < delta) {
                            delta = minv[j];
                            j1 = j;
                        }
                    }
                }
                for (int j = 0; j <= graph.n; j++) {
                    if (used[j]) {
                        u[matching[j]] += delta;
                        v[j] -= delta;
                    } else {
                        minv[j] -= delta;
                    }
                }
                j0 = j1;
            } while (matching[j0] != 0);
            do {
                int j1 = way[j0];
                matching[j0] = matching[j1];
                j0 = j1;
            } while (j0 != 0);
        }

        int answerCost = -v[0];
        int[] result = new int[graph.n];
        for (int j = 1; j < graph.n + 1; j++) {
            result[matching[j] - 1] = j;
        }

        writer.println(Math.abs(answerCost));
        for (int i = 0; i < result.length; i++) {
            writer.write(Integer.toString(i + 1));
            writer.write(" ");
            writer.write(Integer.toString(result[i]));
            writer.write("\n");
        }
        writer.flush();
        writer.close();
    }

    class Matrix {
        int n;
        int[][] matrix, indexEdges;

        Matrix() {
            n = nextInt();
            matrix = new int[n + 1][n + 1];
            indexEdges = new int[n + 1][n + 1];
            for (int i = 1; i < n + 1; i++) {
                for (int j = 1; j < n + 1; j++) {
                    indexEdges[i][j] = i + 1;
                    matrix[i][j] = nextInt();
                }
            }
        }
    }

    int nextInt() {
        try {
            scanner.nextToken();
            return (int) scanner.nval;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return -42;
    }
}

