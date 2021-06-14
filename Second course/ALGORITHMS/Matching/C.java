import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.TreeSet;

/**
 * @author Michael Gerasimov
 * start: 07.03.2021
 * @version -
 */
public class C {
    static StreamTokenizer scanner = new StreamTokenizer(new BufferedReader(new InputStreamReader(System.in)));
    static PrintWriter writer = new PrintWriter(System.out);
    static int[] px;
    static int[] py;
    static boolean[] visited;
    static BipartiteGraph graph;
    static boolean[] visitBoy;
    static boolean[] visitGirl;

    public static void main(String[] args) {
        StringBuilder stringBuilder = new StringBuilder();
        int k = nextInt();
        for (int ki = 0; ki < k; ki++) {
            graph = new BipartiteGraph();
            fordFulkerson();

            visitBoy = new boolean[graph.sizeOfLeft];
            visitGirl = new boolean[graph.sizeOfRight];
            TreeSet<Integer> Boy = new TreeSet<>();
            TreeSet<Integer> Girl = new TreeSet<>();

            for (int i = 0; i < graph.sizeOfLeft; i++) {
                if (px[i] == -1) {
                    dfsBoy(i);
                }
            }

            for (int i = 0; i < graph.sizeOfLeft; i++) {
                if (visitBoy[i]) {
                     Boy.add(i);
                }
            }
            for (int i = 0; i < graph.sizeOfRight; i++) {
                if (!visitGirl[i]) {
                    Girl.add(i);
                }
            }

            stringBuilder.append(Boy.size() + Girl.size());
            stringBuilder.append("\n");
            stringBuilder.append(Boy.size());
            stringBuilder.append(" ");
            stringBuilder.append(Girl.size());
            stringBuilder.append("\n");
            for (int i : Boy)  {
                stringBuilder.append(i + 1);
                stringBuilder.append(" ");
            }
            stringBuilder.append("\n");
            for (int i : Girl) {
                stringBuilder.append(i + 1);
                stringBuilder.append(" ");
            }
            stringBuilder.append("\n\n");
        }
        writer.write(String.valueOf(stringBuilder));
        writer.flush();
        writer.close();
    }

    public static void fordFulkerson() {
        px = new int[graph.sizeOfLeft];
        Arrays.fill(px, -1);
        py = new int[graph.sizeOfRight];
        Arrays.fill(py, -1);
        visited = new boolean[graph.sizeOfLeft];
        boolean isPath = true;
        while (isPath) {
            isPath = false;
            Arrays.fill(visited, false);
            for (int i = 0; i < graph.sizeOfLeft; i++) {
                if (px[i] == -1) {
                    if (dfs(i)) {
                        isPath = true;
                    }
                }
            }
        }
    }

    public static boolean dfs(int x) {
        if (visited[x]) {
            return false;
        }
        visited[x] = true;

        for (int y: graph.edgesLeft.get(x)) {
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

    public static void dfsBoy(int x) {
        visitBoy[x] = true;
        for (int y: graph.edgesLeft.get(x)) {
            if (px[x] != y) {
                if (!visitGirl[y]) {
                    dfsGirl(y);
                }
            }
        }
    }

    public static void dfsGirl(int x) {
        visitGirl[x] = true;
        for (int y : graph.edgesRight.get(x)) {
            if (py[x] == y) {
                if (!visitBoy[y]) {
                    dfsBoy(y);
                }
            }
        }
    }

    static class BipartiteGraph {

        int sizeOfRight;
        int sizeOfLeft;

        ArrayList<ArrayList<Integer>> edgesRight;
        ArrayList<ArrayList<Integer>> edgesLeft;

        BipartiteGraph() {
            sizeOfLeft = nextInt();
            sizeOfRight = nextInt();

            edgesRight = new ArrayList<>(sizeOfRight);
            edgesLeft = new ArrayList<>(sizeOfLeft);
            for (int i = 0; i < Math.max(sizeOfLeft, sizeOfRight); i++) {
                if (i < sizeOfLeft) {
                    edgesLeft.add(new ArrayList<>());
                }
                if (i < sizeOfRight) {
                    edgesRight.add(new ArrayList<>());
                }
            }

            for (int i = 0; i < sizeOfLeft; i++) {
                for (int j = 0; j < sizeOfRight; j++) {
                    edgesLeft.get(i).add(j);
                    edgesRight.get(j).add(i);
                }
            }

            for (int i = 0; i < sizeOfLeft; i++) {
                int x = nextInt() - 1;
                while (x != -1) {
                    edgesLeft.get(i).remove((Integer) x);
                    edgesRight.get(x).remove((Integer) i);
                    x = nextInt() - 1;
                }
            }
        }
    }

    static int nextInt() {
        try {
            scanner.nextToken();
            return (int) scanner.nval;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return -42;
    }
}

