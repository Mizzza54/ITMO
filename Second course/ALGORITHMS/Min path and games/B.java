package B;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 10.11.2020
 * @version -
 */
public class B {
    static Graph Graph;

    public static void main(String[] args) {
        Graph = new Graph();
        Dijkstra(1, Graph);
    }

    public static void Dijkstra(int start, Graph Graph) {
        int[] Distance = new int[Graph.TheNumberOfVertex + 1];
        for (int v: Graph.Vertex) {
            Distance[v] = Integer.MAX_VALUE;
        }
        Distance[start] = 0;

        Queue<Edge> PriorityQueue = new PriorityQueue<>();
        PriorityQueue.add(new Edge(1, 1, 0));

        while (!PriorityQueue.isEmpty()) {
            Edge curEdge = PriorityQueue.poll();
            if (Distance[curEdge.to] < curEdge.weight) {
                continue;
            }

            for (Edge e: Graph.ListOfEdgesForEachVertex.get(curEdge.to)) {
                if (Distance[curEdge.to] + e.weight < Distance[e.to]) {
                    Distance[e.to] = Distance[curEdge.to] + e.weight;
                    PriorityQueue.add(new Edge(curEdge.to, e.to, Distance[e.to]));
                }
            }
        }

        for (int i = 1; i < Distance.length; i++) {
            System.out.print(Distance[i] + " ");
        }
    }
}

class Graph {
    FastReader scanner = new FastReader(System.in);

    int TheNumberOfVertex;
    int TheNumberOfEdges;
    TreeSet<Integer> Vertex = new TreeSet<>();
    ArrayList<HashSet<Edge>> ListOfEdgesForEachVertex = new ArrayList<>();

    public Graph() {
        TheNumberOfVertex = scanner.nextInt();
        TheNumberOfEdges = scanner.nextInt();
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
            int weight = scanner.nextInt();
            ListOfEdgesForEachVertex.get(from).add(new Edge(from, to, weight));
            ListOfEdgesForEachVertex.get(to).add(new Edge(to, from, weight));
        }
    }
}

class Edge implements Comparable<Edge> {
    Integer from;
    Integer to;
    Integer weight;

    Edge(Integer left, Integer right, Integer weight) {
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
        return Integer.compare(this.weight, o.weight);
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