package H;

import java.io.*;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 20.10.2020
 * @version -
 */
public class H {
    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("avia.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    static Graph Graph;
    static boolean[] visited;

    public static void main(String[] args) throws FileNotFoundException {
        Graph = new Graph();
        visited = new boolean[Graph.TheNumberOfVertex + 1];

        int rightBoard = Graph.MaxWeight;
        int leftBoard = 1;
        while (rightBoard - leftBoard > 1) {
            int middle = (rightBoard + leftBoard) / 2;
            Arrays.fill(visited, false);
            int DFS = CountDfs(1, middle);
            if (DFS == Graph.TheNumberOfVertex) {
                Arrays.fill(visited, false);
                int DFSInvert = CountDfsInvert(1, middle);
                if (DFSInvert == Graph.TheNumberOfVertex) {
                    rightBoard = middle;
                } else {
                    leftBoard = middle;
                }
            } else {
                leftBoard = middle;
            }
        }

        writer.println(rightBoard);
        writer.close();
    }


    public static int CountDfs(int v, int capacity) {
        visited[v] = true;
        int count = 1;
        for (int u = 1; u < Graph.TheNumberOfVertex + 1; u++) {
            if (!visited[u]) {
                if (capacity >= Graph.ListOfEdgesForEachVertex[v][u]) {
                    count += CountDfs(u, capacity);
                }
            }
        }
        return count;
    }

    public static int CountDfsInvert(int v, int capacity) {
        visited[v] = true;
        int count = 1;
        for (int u = 1; u < Graph.TheNumberOfVertex + 1; u++) {
            if (!visited[u]) {
                if (capacity >= Graph.ListOfEdgesForEachVertexInvert[v][u]) {
                    count += CountDfsInvert(u, capacity);
                }
            }
        }
        return count;
    }

}

class Graph {
    FastReader scanner = new FastReader("avia.in");

    int TheNumberOfVertex;
    int[][] ListOfEdgesForEachVertex;
    int[][] ListOfEdgesForEachVertexInvert;
    int MaxWeight = 0;

    public Graph() throws FileNotFoundException {
        TheNumberOfVertex = scanner.nextInt();
        ListOfEdgesForEachVertex = new int[TheNumberOfVertex + 1][TheNumberOfVertex + 1];
        ListOfEdgesForEachVertexInvert = new int[TheNumberOfVertex + 1][TheNumberOfVertex + 1];

        for (int i = 1; i < TheNumberOfVertex + 1; i++) {
            for (int j = 1; j < TheNumberOfVertex + 1; j++) {
                int weight = scanner.nextInt();
                MaxWeight = Math.max(weight, MaxWeight);
                ListOfEdgesForEachVertex[i][j] = weight;
                ListOfEdgesForEachVertexInvert[j][i] = weight;
            }
        }
    }
}

class FastReader {
    BufferedReader br;
    StringTokenizer st;

    FastReader(String input) throws FileNotFoundException {
        br = new BufferedReader(new FileReader(input));
    }

    String next() {
        while (st == null || !st.hasMoreElements()) {
            try {
                st = new StringTokenizer(br.readLine());
            } catch (Exception e) {
                return null;
            }
        }
        return st.nextToken();
    }

    int nextInt() {
        return Integer.parseInt(next());
    }
}
