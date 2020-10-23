package J;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.StringTokenizer;
/**
 * @author Michael Gerasimov
 * start: 18.10.2020
 * @version -
 */
public class J {
    static Graph Graph;
    static long Answer = 0;

    public static void main(String[] args) {
        Graph = new Graph();

        kruskalFindMst();

        System.out.println(Answer);
    }

    public static void kruskalFindMst() {
        DSF DSF = new DSF(Graph.TheNumberOfVertex + 1);
        Arrays.sort(Graph.Edges);
        for (Edge edge : Graph.Edges) {
            if (DSF.union(edge.left, edge.right)) {
                Answer += edge.weight;
            }
        }
    }
}

class Graph {
    FastReader scanner = new FastReader(System.in);

    int TheNumberOfVertex;
    int TheNumberOfEdges;
    Edge[] Edges;

    public Graph() {
        TheNumberOfVertex = scanner.nextInt();
        TheNumberOfEdges = scanner.nextInt();
        Edges = new Edge[TheNumberOfEdges];

        for (int i = 0; i < TheNumberOfEdges; i++) {
            int left = scanner.nextInt();
            int right = scanner.nextInt();
            int weight = scanner.nextInt();
            Edges[i] = new Edge(left, right, weight);
        }
    }
}

class Edge implements Comparable<Edge> {
    int left;
    int right;
    int weight;

    Edge(int left, int right, int weight) {
        this.left = left;
        this.right = right;
        this.weight = weight;
    }

    @Override
    public int compareTo(Edge obj) {
        return Integer.compare(weight, obj.weight);
    }
}

class DSF {
    int[] parents;
    long[] rank;

    DSF(int size) {
        parents = new int[size];
        rank = new long[size];
        for (int i = 0; i < size; i++) {
            parents[i] = i;
            rank[i] = 0;
        }
    }

    int get(int x) {
        if (parents[x] != x) {
            parents[x] = get(parents[x]);
        }
        return parents[x];
    }

    boolean union(int x, int y) {
        x = get(x);
        y = get(y);
        if (x == y) {
            return false;
        }

        if (rank[x] == rank[y]) {
            rank[x]++;
        }

        if (rank[x] < rank[y]) {
            parents[x] = y;
        } else {
            parents[y] = x;
        }

        return true;
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