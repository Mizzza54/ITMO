package C;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 12.10.2020
 * @version -
 */
public class C {
    static Graph Graph;
    static boolean[] visited;
    static int time = 0;
    static int[] up;
    static int[] tin;

    static ArrayList<Integer> answer = new ArrayList<>();

    public static void main(String[] args) {
        Graph = new Graph();
        visited = new boolean[Graph.TheNumberOfVertex + 1];
        up = new int[Graph.TheNumberOfVertex + 1];
        tin = new int[Graph.TheNumberOfVertex + 1];

        for (int i = 1; i < Graph.TheNumberOfVertex + 1; i++) {
            if (!visited[i]) {
                dfs(i, -1);
            }
        }

        Collections.sort(answer);
        System.out.println(answer.size());
        for (int i: answer) {
            System.out.print(i + " ");
        }
    }

    public static void dfs(int v, int parent) {
        visited[v] = true;
        time++;
        up[v] = time;
        tin[v] = time;
        int count = 0;
        for (Edge i: Graph.ListOfEdgesForEachVertex.get(v)) {
            if (i.right == parent) {
                continue;
            }
            if (visited[i.right]) {
                up[v] = Math.min(up[v], tin[i.right]);
            }
            if (!visited[i.right]) {
                dfs(i.right, v);
                count++;
                up[v] = Math.min(up[v], up[i.right]);
                if (parent != -1 && up[i.right] >= tin[v] && !answer.contains(v)) {
                    answer.add(v);
                }
            }
        }
        if (parent == -1 && count > 1 && !answer.contains(v)) {
            answer.add(v);
        }
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
            IncidenceMatrix[right][i + 1] = 1;
            ListOfEdgesForEachVertex.get(left).add(new Edge(left, right, i + 1));
            ListOfEdgesForEachVertex.get(right).add(new Edge(right, left, i + 1));
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


