package G;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 22.10.2020
 * @version -
 */
public class G {
    static Graph Graph;
    static Map<String, Boolean> visited = new HashMap<>();
    static Map<String, Integer> Component = new HashMap<>();
    static ArrayList<String> Ord  = new ArrayList<>();
    static int color = 1;

    public static void main(String[] args) {
        Graph = new Graph();
        for (String str: Graph.ListOfEdgesForEachVertex.keySet()) {
            visited.put(str, false);
            Component.put(str, 0);
        }

        //System.out.println(Graph.ListOfEdgesForEachVertex);

        for (String i: Graph.ListOfEdgesForEachVertex.keySet()) {
            if (!visited.get(i)) {
                dfs1(i);
            }
        }

        //System.out.println(Ord);
        Collections.reverse(Ord);
        for (String i: Ord) {
            if (Component.get(i) == 0) {
                dfs2(i);
                color++;
            }
        }

        //System.out.println(Component);

        ArrayList<String> Answer = new ArrayList<>();
        for (String i: Graph.Names) {
            if (Component.get("-" + i) == Component.get("+" + i)) {
                System.out.println(-1);
                System.exit(0);
            }

            if (Component.get("-" + i) < Component.get("+" + i)) {
                Answer.add(i);
            }
        }

        System.out.println(Answer.size());
        for (String i: Answer) {
            System.out.println(i);
        }
    }


    public static void dfs1(String v) {
        visited.put(v, true);
        for (Edge i: Graph.ListOfEdgesForEachVertex.get(v)) {
            if (!visited.get(i.to)) {
                dfs1(i.to);
            }
        }
        Ord.add(v);
    }

    public static void dfs2(String v) {
        Component.put(v, color);
        for (Edge i: Graph.ListOfEdgesForEachVertexInvert.get(v)) {
            if (Component.get(i.to) == 0) {
                dfs2(i.to);
            }
        }
    }
}

class Graph {
    FastReader scanner = new FastReader(System.in);

    int TheNumberOfVertex;
    int TheNumberOfEdges;
    Map<String, ArrayList<Edge>> ListOfEdgesForEachVertex = new HashMap<>();
    Map<String, ArrayList<Edge>> ListOfEdgesForEachVertexInvert = new HashMap<>();
    HashSet<String> Names = new HashSet<>();

    public Graph() {
        TheNumberOfVertex = scanner.nextInt() * 2;
        TheNumberOfEdges = scanner.nextInt();

        for (int i = 0; i < TheNumberOfVertex / 2; i++) {
            String name = scanner.next();
            Names.add(name);
            ListOfEdgesForEachVertex.put("-" + name, new ArrayList<>());
            ListOfEdgesForEachVertex.put("+" + name, new ArrayList<>());
            ListOfEdgesForEachVertexInvert.put("-" + name, new ArrayList<>());
            ListOfEdgesForEachVertexInvert.put("+" + name, new ArrayList<>());
        }

        for (int i = 0; i < TheNumberOfEdges; i++) {
            String NameFrom = scanner.next();
            String arrow = scanner.next();
            String NameTo = scanner.next();
            String InvertNameFrom = "";
            String InvertNameTo = "";

            switch (NameFrom.charAt(0)) {
                case '-':
                    InvertNameFrom = "+" + NameFrom.substring(1);
                    break;
                case '+':
                    InvertNameFrom = "-" + NameFrom.substring(1);
                    break;
            }
            switch (NameTo.charAt(0)) {
                case '-':
                    InvertNameTo = "+" + NameTo.substring(1);
                    break;
                case '+':
                    InvertNameTo = "-" + NameTo.substring(1);
                    break;
            }

            ListOfEdgesForEachVertex.get(NameFrom).add(new Edge(NameFrom, NameTo));
            ListOfEdgesForEachVertex.get(InvertNameTo).add(new Edge(InvertNameTo, InvertNameFrom));

            ListOfEdgesForEachVertexInvert.get(NameTo).add(new Edge(NameTo, NameFrom));
            ListOfEdgesForEachVertexInvert.get(InvertNameFrom).add(new Edge(InvertNameFrom, InvertNameTo));
        }
    }
}

class Edge {
    String from;
    String to;

    Edge(String left, String right) {
        this.from = left;
        this.to = right;
    }

    @Override
    public String toString() {
        return "from = " + from + " to = " + to;
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