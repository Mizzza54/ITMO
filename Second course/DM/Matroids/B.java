package B;

import java.io.*;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 22.12.2020
 * @version -
 */

public class B {
    static Graph Graph;

    public static void main(String[] args) throws IOException {
        Graph = new Graph();

        kruskalFindMst();

        ArrayList<Integer> arrayAnswer = new ArrayList<>();
        for (Edge edge: Graph.Edges) {
            if (!edge.isMST) {
                if (Graph.MaxSumOfDestroy - edge.weight >= 0) {
                    Graph.MaxSumOfDestroy -= edge.weight;
                    arrayAnswer.add(edge.index);
                } else {
                    break;
                }
            }
        }

        FastWriter writer = new FastWriter("destroy.out");
        Collections.sort(arrayAnswer);
        writer.println(arrayAnswer.size());
        for (int i : arrayAnswer) {
            writer.print(i + " ");
        }
        writer.close();
    }

    public static void kruskalFindMst() {
        DSF DSF = new DSF(Graph.TheNumberOfVertex + 1);
        Arrays.sort(Graph.Edges, Comparator.reverseOrder());

        for (Edge edge : Graph.Edges) {
            if (DSF.union(edge.left, edge.right)) {
                edge.isMST = true;
            }
        }
    }
}

class Graph {
    FastScanner scanner = new FastScanner("destroy.in");

    int TheNumberOfVertex;
    int TheNumberOfEdges;
    long MaxSumOfDestroy;
    Edge[] Edges;

    public Graph() throws IOException {
        TheNumberOfVertex = scanner.nextInt();
        TheNumberOfEdges = scanner.nextInt();
        MaxSumOfDestroy = scanner.nextLong();
        Edges = new Edge[TheNumberOfEdges];

        for (int i = 0; i < TheNumberOfEdges; i++) {
            int left = scanner.nextInt();
            int right = scanner.nextInt();
            long weight = scanner.nextLong();
            Edges[i] = new Edge(left, right, weight, i + 1, false);
        }
    }
}

class Edge implements Comparable<Edge> {
    int left;
    int right;
    long weight;
    int index;
    boolean isMST;

    Edge(int left, int right, long weight, int index, boolean isMST) {
        this.left = left;
        this.right = right;
        this.weight = weight;
        this.index = index;
        this.isMST = isMST;
    }

    @Override
    public int compareTo(Edge obj) {
        return Long.compare(this.weight, obj.weight);
    }

    @Override
    public String toString() {
        return "left = " + left + " right = " + right + " weight = " + weight + " index = " + index + " isMST = " + isMST;
    }
}

class DSF {
    int[] parents;
    int[] rank;

    DSF(int size) {
        parents = new int[size];
        rank = new int[size];
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

class FastScanner {
    public StreamTokenizer t;

    FastScanner(String name) throws FileNotFoundException {
        t = new StreamTokenizer( new BufferedReader(new FileReader(name)));
    }

    public int nextInt() throws IOException {
        t.nextToken();
        return (int) t.nval;
    }

    public long nextLong() throws IOException {
        t.nextToken();
        return (long) t.nval;
    }

    public String nextString() throws IOException {
        t.nextToken();
        return t.sval;
    }
}

class FastWriter {
    public PrintWriter writer;

    FastWriter(String name) throws FileNotFoundException {
        this.writer = new PrintWriter(name);
    }

    public void print(String str) {
        writer.print(str);
    }

    public void println(String str) {
        writer.println(str);
    }

    public void print(int str) {
        writer.print(str);
    }

    public void println(int str) {
        writer.println(str);
    }

    public void print(long str) {
        writer.print(str);
    }

    public void println(long str) {
        writer.println(str);
    }

    public void close() {
        writer.close();
    }
}