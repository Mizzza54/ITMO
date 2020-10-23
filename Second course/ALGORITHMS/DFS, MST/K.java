package K;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 19.10.2020
 * @version -
 */
public class K {

    public static void main(String[] args) {
        Graph Graph = new Graph();

        if (StartCountDfs(1, Graph) != Graph.TheNumberOfVertex) {
            System.out.println("NO");
        } else {
            long  result = findMST(Graph, Graph.TheNumberOfVertex,1);
            System.out.println("YES");
            System.out.println(result);
        }
    }

    public static long findMST(Graph Graph, int n, int root) {
        long result = 0;
        int[] minEdge = new int[n + 1]; // создаем массив минимумов, входящих в каждую компоненту, инициализируем бесконечностью.
        Arrays.fill(minEdge, Integer.MAX_VALUE);
        for (HashSet<Edge> set: Graph.ListOfEdges) {
            for (Edge e : set) {
                minEdge[e.to] = Math.min(e.weight, minEdge[e.to]);
            }
        }
        for (int v = 1; v < n + 1; v++) { //each v∈V∖{ root }
            if (v == root) {
                continue;
            }
            result += minEdge[v]; //веса минимальных ребер точно будут в результате
        }

        Graph ZeroGraph = new Graph(Graph, minEdge); //создаем массив нулевых ребе
        //System.out.println(ZeroGraph);
        if (StartCountDfs(root, ZeroGraph) == Graph.TheNumberOfVertex) { // проверяем, можно ли дойти до всех вершин по нулевым ребрам
            return result;
        }


        //int[] newComponents = new int[n]; // будущие компоненты связности
        Condensation newComponents = new Condensation(ZeroGraph);
        Graph NewGraph = new Graph(newComponents.color, newComponents.Component[root], Graph, newComponents.Component, minEdge);
        result += findMST(NewGraph, NewGraph.TheNumberOfVertex, NewGraph.MainRoot);
        return result;
    }

    public static int CountDfs(int v, Graph Graph, boolean[] visited) {
        visited[v] = true;
        int count = 1;
        for (Edge e: Graph.ListOfEdges.get(v)) {
            if (!visited[e.to]) {
                count += CountDfs(e.to, Graph, visited);
            }
        }

        /*
        5 8
1 2 5
1 5 5
2 3 1
3 2 1
3 4 2
4 5 1
5 4 1
5 2 2
         */

        return count;
    }

    public static int StartCountDfs(int startVertex, Graph Graph) {
        boolean[] visited = new boolean[Graph.TheNumberOfVertex + 1];
        int count = CountDfs(startVertex, Graph, visited);
        return count;
    }
}

class Graph {
    FastReader scanner = new FastReader(System.in);

    int TheNumberOfVertex;
    int TheNumberOfEdges;
    int MainRoot;
    ArrayList<HashSet<Edge>> ListOfEdges;
    ArrayList<HashSet<Edge>> ListOfEdgesInvert = new ArrayList<>();

    Graph () {
        TheNumberOfVertex = scanner.nextInt();
        TheNumberOfEdges = scanner.nextInt();
        MainRoot = 1;
        ListOfEdges = new ArrayList<>(TheNumberOfVertex + 1);

        for (int i = 0; i < TheNumberOfVertex + 1; i++) {
            ListOfEdges.add(new HashSet<>());
            ListOfEdgesInvert.add(new HashSet<>());
        }

        for (int i = 0; i < TheNumberOfEdges; i++) {
            int from = scanner.nextInt();
            int to = scanner.nextInt();
            int weight = scanner.nextInt();

            if (ListOfEdges.get(from) == null) {
                ListOfEdges.set(from, new HashSet<Edge>());
            }

            ListOfEdges.get(from).add(new Edge(from, to, weight));
            ListOfEdgesInvert.get(to).add(new Edge(to, from, weight));
        }
    }

    Graph(int TheNumberOfVertex, int MainRoot, Graph OldGraph, int[] Components, int[] minEdgeInVertex)  {
        this.TheNumberOfVertex = TheNumberOfVertex - 1;
        TheNumberOfEdges = 0;
        this.MainRoot = MainRoot;
        ListOfEdges = new ArrayList<>(TheNumberOfVertex + 1);

        for (int i = 0; i < TheNumberOfVertex + 1; i++) {
            ListOfEdges.add(new HashSet<>());
            ListOfEdgesInvert.add(new HashSet<>());
        }

        for (HashSet<Edge> set: OldGraph.ListOfEdges) {
            for (Edge e : set) {
                int from = Components[e.from];
                int to = Components[e.to];
                if (from != to) {
                    if (ListOfEdges.get(from) == null) {
                        ListOfEdges.set(from, new HashSet<Edge>());
                    }
                    ListOfEdges.get(from).add(new Edge(from, to, e.weight - minEdgeInVertex[e.to]));
                    ListOfEdgesInvert.get(to).add(new Edge(to, from, e.weight - minEdgeInVertex[e.to]));
                    TheNumberOfEdges++;
                }
            }
        }

    }

    Graph(Graph OldGraph, int[] minEdge)  {
        TheNumberOfVertex = OldGraph.TheNumberOfVertex;
        TheNumberOfEdges = OldGraph.TheNumberOfEdges;
        MainRoot = OldGraph.MainRoot;
        ListOfEdges = new ArrayList<>(TheNumberOfVertex + 1);

        for (int i = 0; i < TheNumberOfVertex + 1; i++) {
            ListOfEdges.add(new HashSet<>());
            ListOfEdgesInvert.add(new HashSet<>());
        }

        for (HashSet<Edge> set: OldGraph.ListOfEdges) {
            for (Edge e : set) {
                if (e.weight == minEdge[e.to]) {
                    if (ListOfEdges.get(e.from) == null) {
                        ListOfEdges.set(e.from, new HashSet<Edge>());
                    }

                    ListOfEdges.get(e.from).add(new Edge(e.from, e.to, 0));
                    ListOfEdgesInvert.get(e.to).add(new Edge(e.to, e.from, 0));
                    //ListOfEdges.get(e.from).add(new Edge(e.from, e.to, e.weight - something));
                    //zeroEdges.add(new Edge(e.from, e.to, e.weight - something)); // e1 - ребро е, уменьшенное на минимальный вес, входящий в e.to
                }
            }
        }
    }

    @Override
    public String toString() {
        System.out.println("TheNumberOfVertex = " + TheNumberOfVertex + "   TheNumberOfEdges = " + TheNumberOfEdges + "    MainRoot = " + MainRoot);
        for (int i = 1; i < TheNumberOfVertex + 1; i++) {
            for (Edge e: ListOfEdges.get(i)) {
                System.out.println(e);
            }
            System.out.println("\n\n");
        }
        return "";
    }
}

class Condensation {
    Graph Graph;
    boolean[] visited;
    int[] Component;
    ArrayList<Integer> Ord  = new ArrayList<>();
    int color = 1;

    Condensation(Graph Graph) {
        this.Graph = Graph;
        visited = new boolean[Graph.TheNumberOfVertex + 1];
        Component = new int[Graph.TheNumberOfVertex + 1];

        for (int i = 1; i < Graph.TheNumberOfVertex + 1; i++) {
            if (!visited[i]) {
                dfs1(i);
            }
        }

        Collections.reverse(Ord);
        for (int i: Ord) {
            if (Component[i] == 0) {
                dfs2(i);
                color++;
            }
        }
    }

    public void dfs1(int v) {
        visited[v] = true;
        for (Edge i: Graph.ListOfEdges.get(v)) {
            if (!visited[i.to]) {
                dfs1(i.to);
            }
        }
        Ord.add(v);
    }

    public void dfs2(int v) {
        Component[v] = color;
        for (Edge i: Graph.ListOfEdgesInvert.get(v)) {
            if (Component[i.to] == 0) {
                dfs2(i.to);
            }
        }
    }
}

class Edge implements Comparable<Edge> {
    int from;
    int to;
    int weight;

    Edge(int from, int to, int weight) {
        this.from = from;
        this.to = to;
        this.weight = weight;
    }

    @Override
    public int compareTo(Edge obj) {
        return Integer.compare(weight, obj.weight);
    }

    @Override
    public String toString() {
        return "from = " + from + " to = " + to + " weight = " + weight;
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