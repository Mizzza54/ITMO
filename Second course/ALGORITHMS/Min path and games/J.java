package J;

import java.io.*;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 13.11.2020
 * @version -
 */
public class J {
    static ArrayList<Integer> topSort = new ArrayList<>();

    public static void main(String[] args) throws FileNotFoundException {
        Graph Graph = new Graph();
        TopologicalSort(Graph);

        int[] GrandyFunction = new int[Graph.TheNumberOfVertex + 1];
        Arrays.fill(GrandyFunction, 100000);
        for (int u: topSort) {
            int min = 0;
            TreeSet<Integer> UsedValues = new TreeSet<>();
            for (Edge e: Graph.ListOfEdgesForEachVertexInvert.get(u)) {
                int v = e.from;
                UsedValues.add(GrandyFunction[v]);
            }

            for (Edge e: Graph.ListOfEdgesForEachVertex.get(u)) {
                int v = e.to;
                UsedValues.add(GrandyFunction[v]);
            }

            //System.out.println(UsedValues);

            while (UsedValues.contains(min)) {
                min++;
            }

            GrandyFunction[u] = min;
        }

        for (int i = 1; i < GrandyFunction.length; i++) {
            System.out.println(GrandyFunction[i]);
        }
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
    FastReader scanner = new FastReader(System.in);

    int TheNumberOfVertex;
    int TheNumberOfEdges;
    TreeSet<Integer> Vertex = new TreeSet<>();
    ArrayList<HashSet<Edge>> ListOfEdgesForEachVertex = new ArrayList<>();
    ArrayList<HashSet<Edge>> ListOfEdgesForEachVertexInvert = new ArrayList<>();


    public Graph() throws FileNotFoundException {
        TheNumberOfVertex = scanner.nextInt();
        TheNumberOfEdges = scanner.nextInt();

        ListOfEdgesForEachVertex.add(new HashSet<>());
        ListOfEdgesForEachVertexInvert.add(new HashSet<>());
        for (int i = 0; i < TheNumberOfVertex + 1; i++) {
            if (i != 0) {
                Vertex.add(i);
            }
            ListOfEdgesForEachVertex.add(new HashSet<>());
            ListOfEdgesForEachVertexInvert.add(new HashSet<>());
        }

        for (int i = 0; i < TheNumberOfEdges; i++) {
            int from = scanner.nextInt();
            int to = scanner.nextInt();
            ListOfEdgesForEachVertex.get(from).add(new Edge(from, to));
            ListOfEdgesForEachVertexInvert.get(to).add(new Edge(to, from));
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

    FastReader(InputStream input) throws FileNotFoundException {
        br = new BufferedReader(new InputStreamReader(input));
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
