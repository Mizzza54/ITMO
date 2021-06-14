import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Scanner;
import java.util.TreeSet;

/**
 * @author Michael Gerasimov
 * start: 07.03.2021
 * @version -
 */
public class E {
    Scanner scanner = new Scanner(System.in);
    PrintWriter writer = new PrintWriter(System.out);
    int[] px;
    int[] py;
    boolean[] visited;
    BipartiteGraph graph;

    public static void main(String[] args) {
        E E = new E();
        E.run();
    }

    public void run() {
        graph = new BipartiteGraph();

        if (2 * graph.costB <= graph.costA) {
            writer.println(graph.countFree * graph.costB);
            writer.flush();
            writer.close();
            return;
        }

        fordFulkerson();
        int count = 0;
        for (int i = 0; i < graph.sizeWhite; i++) {
            if (px[i] != -1) {
                count++;
            }
        }

        int answer = count * graph.costA + (graph.countFree - count * 2) * graph.costB;
        writer.println(answer);

        writer.flush();
        writer.close();
    }

    public void fordFulkerson() {
        px = new int[graph.sizeWhite];
        Arrays.fill(px, -1);
        py = new int[graph.sizeBlack];
        Arrays.fill(py, -1);
        visited = new boolean[graph.sizeWhite];
        boolean isPath = true;
        while (isPath) {
            isPath = false;
            Arrays.fill(visited, false);
            for (int i = 0; i < graph.sizeWhite; i++) {
                if (px[i] == -1) {
                    if (dfs(i)) {
                        isPath = true;
                    }
                }
            }
        }
    }

    public boolean dfs(int x) {
        if (visited[x]) {
            return false;
        }
        visited[x] = true;

        for (int y: graph.edgesWhite.get(x)) {
            if (py[y] == -1) {
                py[y] = x;
                px[x] = y;
                return true;
            } else {
                if (dfs(py[y])) {
                    py[y] = x;
                    px[x] = y;
                    return true;
                }
            }
        }

        return false;
    }

    class BipartiteGraph {

        int n, m, costA, costB, countFree, sizeWhite, sizeBlack;
        char[][] boardInput;

        ArrayList<TreeSet<Integer>> edgesWhite;
        ArrayList<TreeSet<Integer>> edgesBlack;

        BipartiteGraph() {
            n = scanner.nextInt();
            m = scanner.nextInt();
            costA = scanner.nextInt();
            costB = scanner.nextInt();
            sizeWhite = n * m;
            sizeBlack = n * m;
            countFree = 0;

            edgesWhite = new ArrayList<>();
            edgesBlack = new ArrayList<>();

            for (int i = 0; i < n * m; i++) {
                edgesWhite.add(new TreeSet<>());
                edgesBlack.add(new TreeSet<>());
            }

            boardInput = new char[n][m];
            for (int i = 0; i < n; i++) {
                String line = scanner.next();
                boardInput[i] = line.toCharArray();
            }
            
            /**
             * (i + j) % 2 == 0 -> White
             * (i + j) % 2 != 0 -> Black
             */
            for (int i = 0; i < n; i++) {
                for (int j  = 0; j < m; j++) {
                    if ((i + j) % 2 == 0 && boardInput[i][j] == '*') {
                        //sizeWhite++;
                        addEdgeWhite(i, j);
                    } else {
                        //sizeBlack++;
                        addEdgeBlack(i, j);
                    }
                }
            }
        }

        int coordinatesToVertex(int i, int j) {
            return i * m + j;
        }

        /**
         * up -> down -> left -> right
         */
        void addEdgeWhite(int i, int j) {
            if (!isValidCoordinates(i, j)) {
                return;
            }

            countFree++;

            if (isValidCoordinates(i - 1, j)) {
                edgesWhite.get(coordinatesToVertex(i, j)).add(coordinatesToVertex(i - 1, j));
                edgesBlack.get(coordinatesToVertex(i - 1, j)).add(coordinatesToVertex(i, j));
            }

            if (isValidCoordinates(i + 1, j)) {
                edgesWhite.get(coordinatesToVertex(i, j)).add(coordinatesToVertex(i + 1, j));
                edgesBlack.get(coordinatesToVertex(i + 1, j)).add(coordinatesToVertex(i, j));
            }

            if (isValidCoordinates(i, j - 1)) {
                edgesWhite.get(coordinatesToVertex(i, j)).add(coordinatesToVertex(i, j - 1));
                edgesBlack.get(coordinatesToVertex(i, j - 1)).add(coordinatesToVertex(i, j));
            }

            if (isValidCoordinates(i, j + 1)) {
                edgesWhite.get(coordinatesToVertex(i, j)).add(coordinatesToVertex(i, j + 1));
                edgesBlack.get(coordinatesToVertex(i, j + 1)).add(coordinatesToVertex(i, j));
            }
        }

        void addEdgeBlack(int i, int j) {
            if (!isValidCoordinates(i, j)) {
                return;
            }

            countFree++;

            if (isValidCoordinates(i - 1, j)) {
                edgesBlack.get(coordinatesToVertex(i, j)).add(coordinatesToVertex(i - 1, j));
                edgesWhite.get(coordinatesToVertex(i - 1, j)).add(coordinatesToVertex(i, j));
            }

            if (isValidCoordinates(i + 1, j)) {
                edgesBlack.get(coordinatesToVertex(i, j)).add(coordinatesToVertex(i + 1, j));
                edgesWhite.get(coordinatesToVertex(i + 1, j)).add(coordinatesToVertex(i, j));
            }

            if (isValidCoordinates(i, j - 1)) {
                edgesBlack.get(coordinatesToVertex(i, j)).add(coordinatesToVertex(i, j - 1));
                edgesWhite.get(coordinatesToVertex(i, j - 1)).add(coordinatesToVertex(i, j));
            }

            if (isValidCoordinates(i, j + 1)) {
                edgesBlack.get(coordinatesToVertex(i, j)).add(coordinatesToVertex(i, j + 1));
                edgesWhite.get(coordinatesToVertex(i, j + 1)).add(coordinatesToVertex(i, j));
            }
        }

        boolean isValidCoordinates (int i, int j) {
            return 0 <= i && i < n && 0 <= j && j < m && boardInput[i][j] == '*';
        }
    }
}

