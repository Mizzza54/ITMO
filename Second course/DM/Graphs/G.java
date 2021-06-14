package G;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 07.12.2020
 * @version -
 */
public class G {
    public static int Count = 0;

    public static void main(String[] args) {
        Graph Graph = new Graph();
        ArrayList<Integer> Order = new ArrayList<>();
        Collections.sort(Graph.Degree);

        for (DegreeAndVertex element: Graph.Degree) {
            if (element.degree > Count) {
                Count = element.degree;
            }

            if (element.vertex != 0) {
                Order.add(element.vertex);
            }
        }

        if (Count % 2 == 0) {
            System.out.println(Count + 1);
        } else {
            System.out.println(Count);
        }

        int[] Answer = greedyColoring(Graph, Order);

        for (int u = 1; u < Graph.TheNumberOfVertex + 1; u++) {
            System.out.println(Answer[u]);
        }
    }

    public static int[] greedyColoring(Graph Graph, ArrayList<Integer> Order) {
        int n = Graph.TheNumberOfVertex + 1;
        Queue<Integer> QueueOrder = new ArrayDeque<>();
        QueueOrder.add(Order.get(0));

        int[] Answer = new int[n];
        Arrays.fill(Answer, -1);
        Answer[Order.get(0)]  = 1;

        ArrayList<Queue<Integer>> Colors = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            Colors.add(new ArrayDeque<>());
        }
        for (Queue<Integer> set: Colors) {
            for (int i = 1; i < Count + 3; i++) {
                set.add(i);
            }
        }


        for (Edge edge: Graph.ListOfEdges.get(Order.get(0))) {
            Colors.get(edge.to).remove(1);
        }

        while (!QueueOrder.isEmpty()) {
            int u = QueueOrder.poll();
            for (Edge edge: Graph.ListOfEdges.get(u)) {
                if (Answer[edge.to] == -1) {
                    QueueOrder.add(edge.to);

                    Answer[edge.to] = Colors.get(edge.to).poll();
                    Colors.get(edge.to).remove(Answer[edge.to]);

                    for (Edge edgeOfTo: Graph.ListOfEdges.get(edge.to)) {
                        Colors.get(edgeOfTo.to).remove(Answer[edge.to]);
                    }
                }
            }
        }
        return Answer;
    }
}

class Graph {
    FastReader scanner = new FastReader(System.in);

    int TheNumberOfVertex;
    int TheNumberOfEdges;
    ArrayList<HashSet<Edge>> ListOfEdges;
    ArrayList<DegreeAndVertex> Degree;

    Graph () {
        TheNumberOfVertex = scanner.nextInt();
        TheNumberOfEdges = scanner.nextInt();
        ListOfEdges = new ArrayList<>(TheNumberOfVertex + 1);
        Degree = new ArrayList<>(TheNumberOfVertex + 1);

        for (int i = 0; i < TheNumberOfVertex + 1; i++) {
            ListOfEdges.add(new HashSet<>());
            Degree.add(new DegreeAndVertex(i, 0));
        }

        for (int i = 0; i < TheNumberOfEdges; i++) {
            int from = scanner.nextInt();
            int to = scanner.nextInt();

            ListOfEdges.get(from).add(new Edge(from, to));
            ListOfEdges.get(to).add(new Edge(to, from));
            Degree.get(from).add();
            Degree.get(to).add();
        }
    }
}

class Edge {
    int from;
    int to;

    Edge(int from, int to) {
        this.from = from;
        this.to = to;
    }
}

class DegreeAndVertex implements Comparable<DegreeAndVertex> {
    int degree;
    int vertex;

    DegreeAndVertex(int vertex, int degree) {
        this.degree = degree;
        this.vertex = vertex;
    }

    void add() {
        degree++;
    }

    @Override
    public int compareTo(DegreeAndVertex obj) {
        return (-1) * Integer.compare(degree, obj.degree);
    }

    @Override
    public String toString() {
        return "vertex = " + vertex + " degree = " + degree;
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