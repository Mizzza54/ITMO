package K;

import java.io.*;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 16.11.2020
 * @version -
 */
public class K {
    static int[] GrandyFunction;

    public static void main(String[] args) throws FileNotFoundException {
        Graph Graph = new Graph();
        GrandyFunction = new int[Graph.TheNumberOfVertex + 1];
        Arrays.fill(GrandyFunction, -1);
        EvaluationGrandyFunction(Graph.Root, -1, Graph);

        if (GrandyFunction[Graph.Root] == 0) {
            System.out.println(2);
        } else {
            System.out.println(1);
            System.out.println(Solution(Graph.Root, -1, 0, Graph));
        }
    }

    public static void EvaluationGrandyFunction(int v, int parent, Graph Graph) {
        int value = 0;
        for (Edge e: Graph.ListOfEdgesForEachVertex.get(v)) {
            if (e.to == parent) {
                continue;
            }

            if (GrandyFunction[e.to] == -1) {
                EvaluationGrandyFunction(e.to, v, Graph);
            }
            value = value ^ (GrandyFunction[e.to] + 1);
        }
        GrandyFunction[v] = value;
    }

    public static int Solution(int v, int parent, int need, Graph Graph) {
        for (Edge e: Graph.ListOfEdgesForEachVertex.get(v)) {
            if (e.to == parent) {
                continue;
            }
            int xor = need ^ GrandyFunction[v] ^ (GrandyFunction[e.to] + 1);
            if (xor == 0) {
                return e.index;
            } else {
                xor = Solution(e.to, v, xor - 1, Graph);
            }
            if (xor != -42) {
                return xor;
            }
        }
        return -42;
    }
}

class Graph {
    FastReader scanner = new FastReader(System.in);

    int TheNumberOfVertex;
    int Root;
    TreeSet<Integer> Vertex = new TreeSet<>();
    ArrayList<HashSet<Edge>> ListOfEdgesForEachVertex = new ArrayList<>();


    public Graph() throws FileNotFoundException {
        TheNumberOfVertex = scanner.nextInt();
        Root = scanner.nextInt();

        ListOfEdgesForEachVertex.add(new HashSet<>());
        for (int i = 0; i < TheNumberOfVertex + 1; i++) {
            if (i != 0) {
                Vertex.add(i);
            }
            ListOfEdgesForEachVertex.add(new HashSet<>());
        }

        for (int i = 0; i < TheNumberOfVertex - 1; i++) {
            int from = scanner.nextInt();
            int to = scanner.nextInt();
            ListOfEdgesForEachVertex.get(from).add(new Edge(from, to, i + 1));
            ListOfEdgesForEachVertex.get(to).add(new Edge(to, from, i + 1));
        }
    }
}

class Edge {
    Integer from;
    Integer to;
    Integer index;

    Edge(Integer left, Integer right, Integer index) {
        this.from = left;
        this.to = right;
        this.index = index;
    }

    @Override
    public String toString() {
        return "from = " + from + " to = " + to;
    }
}

class FastReader {
    BufferedReader br;
    StringTokenizer st;

    FastReader(InputStream input) throws FileNotFoundException {
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

    long nextLong() {
        return Long.parseLong(next());
    }
}
