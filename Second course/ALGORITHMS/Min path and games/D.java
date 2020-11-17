package D;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 15.11.2020
 * @version -
 */
public class D {

    public static long INFINITY = (long) 5e18;

    public static void main(String[] args) {
        Graph Graph = new Graph();
        FordBellman(Graph);
    }

    public static void FordBellman(Graph Graph) {
        int start = Graph.StartVertex;
        long[][] Distance = new long[Graph.TheNumberOfEdgesInMinPath + 1][Graph.TheNumberOfVertex + 1];
        for (int v: Graph.Vertex) {
            for (int i = 0; i < Graph.TheNumberOfEdgesInMinPath + 1; i++) {
                Distance[i][v] = INFINITY;
            }
        }
        Distance[0][start] = 0;

        for (int i = 1; i < Graph.TheNumberOfEdgesInMinPath + 1; i++) {
            for (int u : Graph.Vertex) {
                for (Edge e : Graph.ListOfEdgesForEachVertex.get(u)) {
                    int v = e.to;
                    if (Distance[i - 1][u] != INFINITY) {
                        Distance[i][v] = Math.min(Distance[i][v], Distance[i - 1][u] + e.weight);
                    }
                }
            }
        }

        for (int i = 1; i < Graph.TheNumberOfVertex + 1; ++i) {
            if (Distance[Graph.TheNumberOfEdgesInMinPath][i] == INFINITY) {
                System.out.println(-1);
            } else {
                System.out.println(Distance[Graph.TheNumberOfEdgesInMinPath][i]);
            }
        }
    }
}

class Graph {
    FastReader scanner = new FastReader(System.in);

    int TheNumberOfVertex;
    int TheNumberOfEdges;
    int TheNumberOfEdgesInMinPath;
    int StartVertex;
    TreeSet<Integer> Vertex = new TreeSet<>();
    ArrayList<HashSet<Edge>> ListOfEdgesForEachVertex = new ArrayList<>();


    public Graph() {
        TheNumberOfVertex = scanner.nextInt();
        TheNumberOfEdges = scanner.nextInt();
        TheNumberOfEdgesInMinPath = scanner.nextInt();
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
            long weight = scanner.nextLong();
            ListOfEdgesForEachVertex.get(from).add(new Edge(from, to, weight));
        }
    }
}

class Edge implements Comparable<Edge> {
    Integer from;
    Integer to;
    Long weight;

    Edge(Integer left, Integer right, Long weight) {
        this.from = left;
        this.to = right;
        this.weight = weight;
    }

    @Override
    public String toString() {
        return "from = " + from + " to = " + to;
    }

    @Override
    public int compareTo(Edge o) {
        return Long.compare(this.weight, o.weight);
    }
}

class FastReader {
    BufferedReader br;
    StringTokenizer st;

    FastReader(InputStream input) {
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