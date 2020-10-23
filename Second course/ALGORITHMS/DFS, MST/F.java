package F;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 16.10.2020
 * @version -
 */
public class F {
    static Graph Graph;
    static boolean[] visited;
    static int[] Component;
    static ArrayList<Integer> Ord  = new ArrayList<>();
    static int color = 1;

    public static void main(String[] args) {
        Graph = new Graph();
        visited = new boolean[Graph.TheNumberOfVertex + 1];
        Component = new int[Graph.TheNumberOfVertex + 1];

        for (int i = 1; i < Graph.TheNumberOfVertex + 1; i++) {
            if (!visited[i]) {
                dfs1(i);
            }
        }

        Collections.reverse(Ord);
        for (int i: Ord) {
            if (Component[i] == 0) {
                dfs2(i);
                color++;
            }
        }

        HashSet<String> Answer = new HashSet<>();
        for (int i = 1; i < Graph.TheNumberOfVertex + 1; i++) {
            for (Edge e : Graph.ListOfEdgesForEachVertex.get(i)) {
                if (Component[e.left] != Component[e.right]) {
                    int min = Math.min(Component[e.left], Component[e.right]);
                    int max = Math.max(Component[e.left], Component[e.right]);
                    Answer.add(min + " " + max);
                }
            }
        }

        System.out.println(Answer.size());
    }


    public static void dfs1(int v) {
        visited[v] = true;
        for (Edge i: Graph.ListOfEdgesForEachVertex.get(v)) {
            if (!visited[i.right]) {
                dfs1(i.right);
            }
        }
        Ord.add(v);
    }

    public static void dfs2(int v) {
        Component[v] = color;
        for (Edge i: Graph.ListOfEdgesForEachVertexInvert.get(v)) {
            if (Component[i.right] == 0) {
                dfs2(i.right);
            }
        }
    }
}

class Graph {
    FastReader scanner = new FastReader(System.in);

    int TheNumberOfVertex;
    int TheNumberOfEdges;
    ArrayList<HashSet<Edge>> ListOfEdgesForEachVertex = new ArrayList<>();
    ArrayList<HashSet<Edge>> ListOfEdgesForEachVertexInvert = new ArrayList<>();

    public Graph() {
        TheNumberOfVertex = scanner.nextInt();
        TheNumberOfEdges = scanner.nextInt();
        ListOfEdgesForEachVertex.add(new HashSet<>());

        for (int i = 0; i < TheNumberOfVertex + 1; i++) {
            ListOfEdgesForEachVertex.add(new HashSet<>());
            ListOfEdgesForEachVertexInvert.add(new HashSet<>());
        }

        for (int i = 0; i < TheNumberOfEdges; i++) {
            int left = scanner.nextInt();
            int right = scanner.nextInt();
            ListOfEdgesForEachVertex.get(left).add(new Edge(left, right, i + 1));
            ListOfEdgesForEachVertexInvert.get(right).add(new Edge(right, left, i + 1));
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