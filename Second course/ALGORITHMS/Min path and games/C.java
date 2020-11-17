package C;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 11.11.2020
 * @version -
 */
public class C {
    static Graph Graph;

    public static void main(String[] args) {
        Graph = new Graph();
        ArrayList Answer = NegativeCycle(Graph);
        if (Answer == null) {
            System.out.println("NO");
        } else {
            System.out.println("YES");
            System.out.println(Answer.size());
            for (int i = 0; i < Answer.size(); i++) {
                System.out.print(Answer.get(i) + " ");
            }
        }
    }

    public static ArrayList<Integer> NegativeCycle(Graph Graph) {
        long[] Distance = new long[Graph.TheNumberOfVertex + 1];
        int[] Parent = new int[Graph.TheNumberOfVertex + 1];
        int start = -1;
        for (int v: Graph.Vertex) {
            Distance[v] = 0;
            Parent[v] = -1;
        }

        for (int i: Graph.Vertex) {
            start = -1;
            for (int u : Graph.Vertex) {
                for (Edge e : Graph.ListOfEdgesForEachVertex.get(u)) {
                    int v = e.to;
                    if (Distance[v] > Distance[u] + e.weight) {
                        Distance[v] = Distance[u] + e.weight;
                        Parent[v] = u;
                        start = v;
                    }
                }
            }
        }

        //System.out.println(Arrays.toString(Distance));
        //System.out.println(start);

        if (start == -1) {
            return null;
        }

        ArrayList<Integer> Answer = new ArrayList<>();
        for (int i = 0; i < Graph.TheNumberOfVertex; i++) {
            start = Parent[start];
        }

        Answer.add(start);
        int tmp = start;
        start = Parent[start];
        while (tmp != start) {
            Answer.add(start);
            start = Parent[start];
        }

        Collections.reverse(Answer);
        return Answer;
    }
}

class Graph {
    FastReader scanner = new FastReader(System.in);

    int TheNumberOfVertex;
    TreeSet<Integer> Vertex = new TreeSet<>();
    ArrayList<HashSet<Edge>> ListOfEdgesForEachVertex = new ArrayList<>();

    public Graph() {
        TheNumberOfVertex = scanner.nextInt();
        ListOfEdgesForEachVertex.add(new HashSet<>());
        for (int i = 0; i < TheNumberOfVertex + 1; i++) {
            if (i != 0) {
                Vertex.add(i);
            }
            ListOfEdgesForEachVertex.add(new HashSet<>());
        }

        for (int from = 1; from < TheNumberOfVertex + 1; from++) {
            for (int to = 1; to < TheNumberOfVertex + 1; to++) {
                int weight = scanner.nextInt();
                if (weight != 100_000) {
                    ListOfEdgesForEachVertex.get(from).add(new Edge(from, to, weight));
                }
            }
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