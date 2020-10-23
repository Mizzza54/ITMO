package D;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 12.10.2020
 * @version -
 */
public class D {
    static Graph Graph;
    static boolean[] visited;
    static Stack<Integer> stack = new Stack<>();
    static int time = 0;
    static int maxColor = 0;
    static int[] tin;
    static int[] up;
    static int[] colors;

    public static void main(String[] args) {
        Graph = new Graph();
        visited = new boolean[Graph.TheNumberOfVertex + 1];
        tin = new int[Graph.TheNumberOfVertex + 1];
        up = new int[Graph.TheNumberOfVertex + 1];
        colors = new int[Graph.TheNumberOfVertex + 1];

        for (int i = 1; i < Graph.TheNumberOfVertex + 1; i++) {
            if (!visited[i]) {
                dfs(i, -1);
                paint(-2);
            }
        }

        System.out.println(maxColor);
        for (int i = 1; i < colors.length; i++) {
            System.out.print((colors[i]) + " ");
        }
    }

    public static void dfs(int v, int parent) {
        visited[v] = true;
        time++;
        stack.push(v);
        tin[v] = time;
        up[v] = time;
        for (Edge i: Graph.ListOfEdgesForEachVertex.get(v)) {
            if (parent == i.right) {
                continue;
            }

            if (visited[i.right]) {
                up[v] = Math.min(up[v], tin[i.right]);
            }

            if (!visited[i.right]) {
                dfs(i.right, v);
                up[v] = Math.min(up[v], up[i.right]);
                if (up[i.right] > tin[v] && Graph.isMultiEdge.get(i.right + "_" + v) != null && Graph.isMultiEdge.get(i.right + "_" + v) == 1) {
                    paint(i.right);
                }
            }
        }
    }

    public static void paint(int v) {
        maxColor++;
        int last = -1;
        while (!stack.empty() && last != v) {
            colors[stack.peek()] = maxColor;
            last = stack.peek();
            stack.pop();
        }
    }
}

class Graph {
    FastReader scanner = new FastReader(System.in);

    int TheNumberOfVertex;
    int TheNumberOfEdges;
    ArrayList<HashSet<Edge>> ListOfEdgesForEachVertex = new ArrayList<>();
    TreeMap<String, Integer> isMultiEdge = new TreeMap<>();

    public Graph() {
        TheNumberOfVertex = scanner.nextInt();
        TheNumberOfEdges = scanner.nextInt();
        ListOfEdgesForEachVertex.add(new HashSet<>());

        for (int i = 0; i < TheNumberOfVertex; i++) {
            ListOfEdgesForEachVertex.add(new HashSet<>());
        }

        for (int i = 0; i < TheNumberOfEdges; i++) {
            int left = scanner.nextInt();
            int right = scanner.nextInt();
            if (isMultiEdge.get(left + "_" + right) == null) {
                ListOfEdgesForEachVertex.get(left).add(new Edge(left, right, i + 1));
                ListOfEdgesForEachVertex.get(right).add(new Edge(right, left, i + 1));
                isMultiEdge.put(left + "_" + right, 1);
                isMultiEdge.put(right + "_" + left, 1);
            } else if (isMultiEdge.get(left + "_" + right) > 0) {
                isMultiEdge.put(left + "_" + right, isMultiEdge.get(left + "_" + right) + 1);
                isMultiEdge.put(right + "_" + left, isMultiEdge.get(left + "_" + right) + 1);
            }
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


