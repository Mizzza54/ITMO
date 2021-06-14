import java.io.*;
import java.util.*;

public class B {
    PrintWriter writer = new PrintWriter(System.out);

    int INF = 1000000000;

    public boolean[] visited;
    Set<Integer> minCut = new TreeSet<>();

    public static void main(String[] args) {
        B B = new B();
        B.run();
    }

    public void run() {
        Graph graph = new Graph();
        visited = new boolean[graph.numberVertex + 1];

        int startVertex = 1;
        int endVertex = graph.numberVertex;

        while (dfs(startVertex, INF, endVertex, graph) > 0) {
            visited = new boolean[graph.numberVertex + 1];
        }

        visited = new boolean[graph.numberVertex + 1];
        findMinCut(startVertex, graph);

        int maxFlow = 0;
        StringBuilder stringBuilder = new StringBuilder();

        for (Edge edge : graph.listEdges.get(startVertex)) {
            maxFlow += Math.max(edge.getFlow(), 0);
        }

        int count = 0;
        for (int i = 1; i < graph.numberEdges + 1; i++) {
            if (visited[graph.mapIndexToEdge.get(i).getFrom()] ^ visited[graph.mapIndexToEdge.get(i).getTo()]) {
                stringBuilder.append(graph.mapIndexToEdge.get(i).index);
                stringBuilder.append(" ");
                count++;
            }
        }

        writer.write(Integer.toString(count));
        writer.write(" ");
        writer.write(Integer.toString(maxFlow));
        writer.write("\n");
        writer.write(stringBuilder.toString());

        writer.flush();
        writer.close();
    }


    public int dfs(int u, int Cmin, int t, Graph graph) {
        if (u == t) {
            return Cmin;
        }

        visited[u] = true;
        for (Edge edge : graph.listEdges.get(u)) {
            //auto uv = edge(u, v)
            if (!visited[edge.getTo()] && edge.getFlow() < edge.getSpeed()) {
                int delta = dfs(edge.getTo(), Math.min(Cmin, edge.getSpeed() - edge.getFlow()), t, graph);
                if (delta > 0) {
                    edge.setFlow(edge.getFlow() + delta);
                    edge.getBackEdge().setFlow(edge.getBackEdge().getFlow() - delta);
                    return delta;
                }
            }
        }
        return 0;
    }

    void findMinCut(int v, Graph graph) {
        if (visited[v]) {
            return;
        }

        visited[v] = true;

        for (Edge edge : graph.listEdges.get(v)) {
            if (edge.getFlow() == edge.getSpeed()) {
                minCut.add(edge.getIndex());
            }

            if (!visited[edge.getTo()] && edge.getFlow() < edge.getSpeed()) {
                findMinCut(edge.getTo(), graph);
            }
        }
    }

}

class Edge {
    int from, to, speed, index, flow;
    Edge backEdge;

    public Edge(int from, int to, int speed, int index) {
        this.from = from;
        this.to = to;
        this.speed = speed;
        this.index = index;
    }

    public int getFrom() {
        return from;
    }

    public int getTo() {
        return to;
    }

    public void setFlow(int flow) {
        this.flow = flow;
    }

    public int getFlow() {
        return flow;
    }

    public int getSpeed() {
        return speed;
    }

    public void setBackEdge(Edge backEdge) {
        this.backEdge = backEdge;
    }

    public Edge getBackEdge() {
        return backEdge;
    }

    public int getIndex() {
        return index;
    }

    @Override
    public String toString() {
        return "Edge{" +
                "from=" + from +
                ", to=" + to +
                ", speed=" + speed +
                ", index=" + index +
                ", flow=" + flow +
                '}';
    }

    public String toStringAnswer() {
        return Integer.toString(flow);
    }
}

class Graph {
    FastReader scanner = new FastReader(System.in);

    int numberVertex;
    int numberEdges;
    ArrayList<ArrayList<Edge>> listEdges;
    Map<Integer, Edge> mapIndexToEdge;

    public Graph() {
        numberVertex = scanner.nextInt();
        numberEdges = scanner.nextInt();
        listEdges = new ArrayList<>(numberVertex + 1);
        for (int i = 0; i < numberVertex + 1; i++) {
            listEdges.add(new ArrayList<>());
        }
        mapIndexToEdge = new HashMap<>();

        for (int i = 0; i < numberEdges; i++) {
            int from = scanner.nextInt();
            int to = scanner.nextInt();
            int speed = scanner.nextInt();
            Edge edgeA = new Edge(from, to, speed, i + 1);
            Edge edgeB = new Edge(to, from, speed, i + 1);
            edgeA.setBackEdge(edgeB);
            edgeB.setBackEdge(edgeA);
            listEdges.get(from).add(edgeA);
            listEdges.get(to).add(edgeB);
            mapIndexToEdge.put(i + 1, edgeA);
            mapIndexToEdge.put(-(i + 1), edgeB);
        }
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