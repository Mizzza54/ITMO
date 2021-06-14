import java.io.*;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 03.03.2021
 * @version -
 */
public class A {
    StreamTokenizer scanner = new StreamTokenizer(new BufferedReader(new InputStreamReader(System.in)));
    PrintWriter writer = new PrintWriter(System.out);
    int[] px;
    int[] py;
    boolean[] visited;
    BipartiteGraph graph;

    public static void main(String[] args) {
        A A = new A();
        A.run();
    }

    public void run() {
        graph = new BipartiteGraph();
        fordFulkerson();
        int count = 0;
        for (int i = 0; i < graph.sizeOfLeft; i++) {
            if (px[i] != -1) {
                count++;
            }
        }

        writer.println(count);

        for (int i = 0; i < graph.sizeOfLeft; i++) {
            if (px[i] != -1) {
                writer.print(i + 1);
                writer.print(" ");
                writer.println(px[i] + 1);
            }
        }
        writer.flush();
        writer.close();
    }

    public void fordFulkerson() {
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

    public boolean dfs(int x) {
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

    class BipartiteGraph {

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
                int x = nextInt() - 1;
                while (x != -1) {
                    edgesLeft.get(i).add(x);
                    edgesRight.get(x).add(i);
                    x = nextInt() - 1;
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
