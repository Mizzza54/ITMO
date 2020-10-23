package A;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 11.10.2020
 * @version -
 */
public class A {
    static FastReader scanner = new FastReader(System.in);
    static Graph Graph;
    static Colors[] colors;
    static boolean[] visited;

    static ArrayList<Integer> answer = new ArrayList<>();

    public static void main(String[] args) {
        Graph = new Graph();
        colors = new Colors[Graph.TheNumberOfVertex + 1];
        Arrays.fill(colors, Colors.white);
        visited = new boolean[Graph.TheNumberOfVertex + 1];

        for (int i = 1; i < Graph.TheNumberOfVertex + 1; i++) {
            dfsCycle(i);
        }
        TopologicalSort();

        for (int i: answer) {
            System.out.print(i + " ");
        }
    }

    public static void dfs(int u) {
        visited[u] = true;
        for (Edge i: Graph.ListOfEdgesForEachVertex.get(u)) {
            if (!visited[i.right]) {
                dfs(i.right);
            }
        }
        answer.add(u);
    }

    public static void TopologicalSort() {
        for (int v : Graph.Vertex) {
            if (!visited[v]) {
                dfs(v);
            }
        }
        Collections.reverse(answer);
    }

    public static void dfsCycle(int v) {
        colors[v] = Colors.grey;
        for (Edge i: Graph.ListOfEdgesForEachVertex.get(v)) {
            if (colors[i.right] == Colors.white) {
                dfsCycle(i.right);
            } else if (colors[i.right] == Colors.grey) {
                System.out.println(-1);
                System.exit(0);
            }
        }
        colors[v] = Colors.black;
    }
}

class Graph {
    FastReader scanner = new FastReader(System.in);

    int TheNumberOfVertex;
    int TheNumberOfEdges;
    TreeSet<Integer> Vertex = new TreeSet<>();
    TreeMap<Integer, Edge> Edges = new TreeMap<>();
    public int[][] IncidenceMatrix;
    ArrayList<HashSet<Edge>> ListOfEdgesForEachVertex = new ArrayList<>();

    public Graph() {
        TheNumberOfVertex = scanner.nextInt();
        TheNumberOfEdges = scanner.nextInt();
        IncidenceMatrix = new int[TheNumberOfVertex + 1][TheNumberOfEdges + 1];
        ListOfEdgesForEachVertex.add(new HashSet<>());

        for (int i = 0; i < TheNumberOfVertex; i++) {
            Vertex.add(i + 1);
            ListOfEdgesForEachVertex.add(new HashSet<>());
        }

        for (int i = 0; i < TheNumberOfEdges; i++) {
            int left = scanner.nextInt();
            int right = scanner.nextInt();
            Edges.put(i + 1, new Edge(left, right, i + 1));
            IncidenceMatrix[left][i + 1] = 1;
            IncidenceMatrix[right][i + 1] = -1;
            ListOfEdgesForEachVertex.get(left).add(new Edge(left, right, i + 1));
        }
    }
}

class Edge {
    int index;
    int left;
    int right;

    Edge(int left, int right, int index) {
        this.left = left;
        this.right = right;
        this.index = index;
    }
}

enum Colors {
    white, grey, black;
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
