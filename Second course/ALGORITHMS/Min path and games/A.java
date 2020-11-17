package A;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 10.11.2020
 * @version -
 */
public class A {
    static FastReader scanner = new FastReader(System.in);
    static Graph Graph;

    public static void main(String[] args) {
        Graph = new Graph();
        Floyd(Graph);
    }

    public static void Floyd(Graph Graph) {
        int[][] MinDistance = new int[Graph.IncidenceMatrix.length][];
        System.arraycopy(Graph.IncidenceMatrix, 0, MinDistance, 0, Graph.IncidenceMatrix.length);
        for (int i: Graph.Vertex) {
            for (int u: Graph.Vertex) {
                for (int v: Graph.Vertex) {
                    MinDistance[u][v] = Math.min(MinDistance[u][v], MinDistance[u][i] + MinDistance[i][v]);
                }
            }
        }

        for (int i = 1; i < MinDistance.length; i++) {
            for (int j = 1; j < MinDistance[i].length; j++) {
                System.out.print(MinDistance[i][j] + " ");
            }
            System.out.println();
        }
    }
}

class Graph {
    FastReader scanner = new FastReader(System.in);

    int TheNumberOfVertex;
    TreeSet<Integer> Vertex = new TreeSet<>();
    public int[][] IncidenceMatrix;
    ArrayList<HashSet<Edge>> ListOfEdgesForEachVertex = new ArrayList<>();

    public Graph() {
        TheNumberOfVertex = scanner.nextInt();
        IncidenceMatrix = new int[TheNumberOfVertex + 1][TheNumberOfVertex + 1];
        ListOfEdgesForEachVertex.add(new HashSet<>());

        for (int i = 0; i < TheNumberOfVertex; i++) {
            Vertex.add(i + 1);
            ListOfEdgesForEachVertex.add(new HashSet<>());
        }

        for (int from = 1; from < TheNumberOfVertex + 1; from++) {
            for (int to = 1; to < TheNumberOfVertex + 1; to++) {
                IncidenceMatrix[from][to] = scanner.nextInt();
                ListOfEdgesForEachVertex.get(from).add(new Edge(from, to));
            }
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
}