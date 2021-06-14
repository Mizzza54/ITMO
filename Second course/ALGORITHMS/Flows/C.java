import java.io.*;
import java.util.*;

public class C {
    PrintWriter writer = new PrintWriter(System.out);

    int INF = 900000;

    public boolean[] visited;
    boolean isPathFound = false;

    public static void main(String[] args) {
        C C = new C();
        C.run();

    }

    public void run() {
        Graph graph = new Graph();
        visited = new boolean[graph.numberVertex + 1];

        int startVertex = graph.startVertex;
        int endVertex = graph.endVertex;

        while (dfs(startVertex, INF, endVertex, graph) > 0) {
            visited = new boolean[graph.numberVertex + 1];
        }

        int maxFlow = 0;
        for (Edge edge : graph.listEdges.get(startVertex)) {
            if (edge.getIndex() > 0) {
                maxFlow += Math.max(edge.getFlow(), 0);
            }
        }

        if (maxFlow < 2) {
            writer.write("NO");
            writer.flush();
            writer.close();
            return;
        }

        List<Integer> pathA = new ArrayList<>();
        List<Integer> pathB = new ArrayList<>();
        pathA.add(startVertex);
        pathB.add(startVertex);

        boolean isPathExists = false;
        isPathFound = false;
        findPath(startVertex, endVertex, graph, pathA);
        isPathExists = isPathFound;
        isPathFound = false;
        findPath(startVertex, endVertex, graph, pathB);
        isPathExists = isPathExists & isPathFound;

        if (!isPathExists) {
            writer.write("NO");
            writer.flush();
            writer.close();
            return;
        }

        writer.write("YES\n");

        for (int i: pathA) {
            writer.write(i + " ");
        }
        writer.write("\n");

        for (int i: pathB) {
            writer.write(i + " ");
        }
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

    void findPath(int u, int t, Graph graph, List<Integer> path) {

        if (u == t) {
            isPathFound = true;
            return;
        }

        for (Edge edge : graph.listEdges.get(u)) {
            if (edge.getIndex() <= 0) {
                continue;
            }

            if (!isPathFound && edge.getFlow() == 1) {
                edge.setFlow(0);
                path.add(edge.getTo());
                findPath(edge.getTo(), t, graph, path);
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
        this.flow = 0;
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
    int startVertex, endVertex;
    ArrayList<ArrayList<Edge>> listEdges;
    Map<Integer, Edge> mapIndexToEdge;

    public Graph() {
        numberVertex = scanner.nextInt();
        numberEdges = scanner.nextInt();
        startVertex = scanner.nextInt();
        endVertex = scanner.nextInt();
        listEdges = new ArrayList<>(numberVertex + 1);
        for (int i = 0; i < numberVertex + 1; i++) {
            listEdges.add(new ArrayList<>());
        }
        mapIndexToEdge = new HashMap<>();

        for (int i = 0; i < numberEdges; i++) {
            int from = scanner.nextInt();
            int to = scanner.nextInt();
            int speed = 1;
            Edge edgeA = new Edge(from, to, speed, i + 1);
            Edge edgeB = new Edge(to, from, 0, -(i + 1));
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