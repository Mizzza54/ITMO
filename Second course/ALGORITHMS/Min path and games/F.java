package F;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;
import java.util.jar.JarOutputStream;

/**
 * @author Michael Gerasimov
 * start: 15.11.2020
 * @version -
 */
public class F {
    public static long INFINITY = (long) 5e18;

    public static void main(String[] args) {
        Graph Graph = new Graph();

        ArrayList<Long> array = Dijkstra(Graph.VertexA, Graph);
        long path1 = array.get(Graph.VertexB - 1);  //abc
        long path2 = array.get(Graph.VertexC - 1);  //acb
        long path3 = array.get(Graph.VertexC - 1);  //bac

        if (path1 == INFINITY || path2 == INFINITY) {
            System.out.println(-1);
            System.exit(0);
        }

        array = Dijkstra(Graph.VertexC, Graph);
        path2 += array.get(Graph.VertexB - 1);

        array = Dijkstra(Graph.VertexB, Graph);
        path1 += array.get(Graph.VertexC - 1);

        path3 += array.get(Graph.VertexA - 1);

        long result = Math.min(Math.min(path1, path2), path3);
        System.out.println(result);
    }

    public static ArrayList<Long> Dijkstra(int start, Graph Graph) {
        long[] Distance = new long[Graph.TheNumberOfVertex + 1];
        for (int v: Graph.Vertex) {
            Distance[v] = INFINITY;
        }
        Distance[start] = 0;

        Queue<Edge> PriorityQueue = new PriorityQueue<>();
        PriorityQueue.add(new Edge(start, start, 0L));

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

        ArrayList<Long> Answer = new ArrayList<>();
        for (int i = 1; i < Distance.length; i++) {
            Answer.add(Distance[i]);
        }
        return Answer;
    }
}

class Graph {
    FastReader scanner = new FastReader(System.in);

    int TheNumberOfVertex;
    int TheNumberOfEdges;
    int VertexA, VertexB, VertexC;
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
            long weight = scanner.nextInt();
            ListOfEdgesForEachVertex.get(from).add(new Edge(from, to, weight));
            ListOfEdgesForEachVertex.get(to).add(new Edge(to, from, weight));
        }

        VertexA = scanner.nextInt();
        VertexB = scanner.nextInt();
        VertexC = scanner.nextInt();
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
}