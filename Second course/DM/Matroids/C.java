package C;

import java.io.*;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 29.12.2020
 * @version -
 */

public class C {
    static Graph Graph;
    static boolean[] used;
    static int[] matching;

    public static void main(String[] args) throws IOException {
        Graph = new Graph();

        matching = new int[Graph.TheNumberOfVertex + 1];
        Arrays.fill(matching, -1);

        for (Pair pair: Graph.Weight) {
            used = new boolean[Graph.TheNumberOfVertex + 1];
            dfs(pair.index);
        }

        FastWriter writer = new FastWriter("matching.out");


        int[] answer = new int[matching.length];
        for (int i = 1; i < matching.length; i++) {
            if (matching[i] != -1) {
                answer[matching[i]] = i;
            }
        }

        for (int i = 1; i < matching.length; i++) {
            writer.print(answer[i] + " ");
        }

        writer.close();
    }

    public static boolean dfs(int v) {
        if (used[v]) {
            return false;
        }
        used[v] = true;
        for (Edge e: Graph.Edges.get(v)) {
            int to = e.right;
            if (matching[to] == -1 || dfs(matching[to])) {
                matching[to] = v;
                return true;
            }
        }
        return false;
    }
}

class Graph {
    FastScanner scanner = new FastScanner("matching.in");

    int TheNumberOfVertex;
    ArrayList<ArrayList<Edge>> Edges;
    ArrayList<Pair> Weight;

    public Graph() throws IOException {
        TheNumberOfVertex = scanner.nextInt();
        Weight = new ArrayList<Pair>();
        Edges = new ArrayList<>();

        for (int i = 0; i < TheNumberOfVertex; i++) {
            Weight.add(new Pair(scanner.nextInt(), i + 1));
            Edges.add(new ArrayList<>());
        }
        Edges.add(new ArrayList<>());


        for (int i = 0; i < TheNumberOfVertex; i++) {
            int temp = scanner.nextInt();
            for (int j = 0; j < temp; j++) {
                Edges.get(i + 1).add(new Edge(i + 1, scanner.nextInt()));
            }
        }

        Collections.sort(Weight);
        Collections.reverse(Weight);
    }
}

class Pair implements Comparable<Pair> {
    int weight;
    int index;

    Pair(int weight, int index) {
        this.weight = weight;
        this.index = index;
    }

    @Override
    public int compareTo(Pair o) {
        return Integer.compare(this.weight, o.weight);
    }

    @Override
    public String toString() {
        return "weight = " + weight + " index = " + index;
    }
}

class Edge{
    int left;
    int right;

    Edge(int left, int right) {
        this.left = left;
        this.right = right;
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