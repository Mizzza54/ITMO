package H;

import java.io.*;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 12.11.2020
 * @version -
 */
public class H {
    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("game.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
    static ArrayList<Integer> topSort = new ArrayList<>();

    public static void main(String[] args) throws FileNotFoundException {
        Graph Graph = new Graph();
        TopologicalSort(Graph);

        boolean[] isWinFirst = new boolean[Graph.TheNumberOfVertex + 1];
        for (int u: topSort) {
            for (Edge e: Graph.ListOfEdgesForEachVertex.get(u)) {
                int v = e.to;
                isWinFirst[u] = isWinFirst[u] || !isWinFirst[v];
                if (isWinFirst[u]) {
                    break;
                }
            }
        }

        if (isWinFirst[Graph.StartVertex]) {
            writer.println("First player wins");
        } else {
            writer.println("Second player wins");
        }
        writer.close();
    }

    public static void dfs(int u, Graph Graph, boolean[] visited) {
        visited[u] = true;
        for (Edge i: Graph.ListOfEdgesForEachVertex.get(u)) {
            if (!visited[i.to]) {
                dfs(i.to, Graph, visited);
            }
        }
        topSort.add(u);
    }

    public static void TopologicalSort(Graph Graph) {
        boolean[] visited = new boolean[Graph.TheNumberOfVertex + 1];
        for (int v : Graph.Vertex) {
            if (!visited[v]) {
                dfs(v, Graph, visited);
            }
        }
        //Collections.reverse(topSort);
    }
}

class Graph {
    FastReader scanner = new FastReader("game.in");

    int TheNumberOfVertex;
    int TheNumberOfEdges;
    int StartVertex;
    TreeSet<Integer> Vertex = new TreeSet<>();
    ArrayList<HashSet<Edge>> ListOfEdgesForEachVertex = new ArrayList<>();


    public Graph() throws FileNotFoundException {
        TheNumberOfVertex = scanner.nextInt();
        TheNumberOfEdges = scanner.nextInt();
        StartVertex = scanner.nextInt();

        ListOfEdgesForEachVertex.add(new HashSet<>());
        for (int i = 0; i < TheNumberOfVertex + 1; i++) {
            if (i != 0) {
                Vertex.add(i);
            }
            ListOfEdgesForEachVertex.add(new HashSet<>());
        }

        for (int i = 0; i < TheNumberOfEdges; i++) {
            int from = scanner.nextInt();
            int to = scanner.nextInt();
            ListOfEdgesForEachVertex.get(from).add(new Edge(from, to));
        }
    }
}

class Edge {
    Integer from;
    Integer to;

    Edge(Integer left, Integer right) {
        this.from = left;
        this.to = right;
    }

    @Override
    public String toString() {
        return "from = " + from + " to = " + to;
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

    long nextLong() {
        return Long.parseLong(next());
    }
}