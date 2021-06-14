import java.io.PrintWriter;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 20.04.2021
 * @version -
 */
public class D {
    PrintWriter writer = new PrintWriter(System.out);

    int INF = 1000000000;

    public boolean[] visited;
    Set<Integer> minCut = new TreeSet<>();

    public static void main(String[] args) {
        D D = new D();
        D.run();
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
            maxFlow += Math.max(edge.getFlow(), 0);
        }

        if (maxFlow >= INF) {
            writer.write(Integer.toString(-1));
            writer.flush();
            writer.close();
            return;
        }

        visited = new boolean[graph.numberVertex + 1];
        findMinCut(graph);
        for (int v: minCut) {
            graph.countyMap[Math.abs(v) / graph.m][Math.abs(v) % graph.m].ch = '+';
        }

        writer.write(Integer.toString(maxFlow));
        writer.write("\n");
        for (int i = 0; i < graph.n; i++) {
            for (int j = 0; j < graph.m; j++) {
                writer.write(graph.countyMap[i][j].ch);
            }
            writer.write("\n");
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

    void findMinCut(Graph graph) {

        visited = new boolean[graph.numberVertex + 1];
        dfsVisit(graph.startVertex, graph);
        for (int v = 0; v < graph.numberVertex; v++) {
            if (!visited[v])  {
                continue;
            }

            for (Edge e: graph.listEdges.get(v)) {
                if (!visited[e.to] && e.flow == 1) {
                    minCut.add(e.to);
                    break;
                }
            }
        }
    }

    void dfsVisit(int from, Graph graph) {
        visited[from] = true;
        for (Edge e: graph.listEdges.get(from)) {
            if (!visited[e.to] && e.speed > e.flow) {
                dfsVisit(e.to, graph);
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
    int INF = 1000000000;
    Scanner scanner = new Scanner(System.in);

    int n, m, numberVertex;
    int startVertex, endVertex;
    ArrayList<ArrayList<Edge>> listEdges;
    Map<Integer, Edge> mapIndexToEdge;
    Cell[][] countyMap;
    int countEdges = 0;

    public Graph() {
        n = scanner.nextInt();
        m = scanner.nextInt();
        countyMap = new Cell[n][m];
        mapIndexToEdge = new HashMap<>();
        listEdges = new ArrayList<>();
        listEdges.add(new ArrayList<>());
        for (int i = 0; i < n * m * 2; i++) {
            listEdges.add(new ArrayList<>());
        }
        numberVertex = n * m * 2;
        scanner.nextLine();
        for (int i = 0; i < n; i++) {
            String string = scanner.nextLine();
            for (int j = 0; j < m; j++) {
                char ch = string.charAt(j);
                countyMap[i][j] = new Cell(i, j, ch);
                switch (ch) {
                    case '#':
                        break;
                    case '.':
                        addEdge(indexInEdges(i, j, n, m),
                                indexOutEdges(i, j, m),
                                1);
                        break;
                    case '-':
                        addEdge(indexInEdges(i, j, n, m),
                                indexOutEdges(i, j, m),
                                INF);
                        break;
                    case 'A':
                        startVertex = indexOutEdges(i, j, m);
                        break;
                    case 'B':
                        endVertex = indexInEdges(i, j, n, m);
                        break;
                }
            }
        }

        for (int i = 0; i < n; i++)
            for (int j = 0; j < m; j++) {
                if (countyMap[i][j].ch == '#') {
                    continue;
                }

                int from = indexOutEdges(i, j, m);
                int to = indexInEdges(i, j, n, m);

                if (i + 1 < n && countyMap[i + 1][j].ch != '#') {
                    addEdge(indexOutEdges(i + 1, j, m), to, INF);
                    addEdge(from, indexInEdges(i + 1, j, n, m), INF);
                }

                if (j + 1 < m && countyMap[i][j + 1].ch != '#') {
                    addEdge(indexOutEdges(i, j + 1, m), to, INF);
                    addEdge(from, indexInEdges(i, j + 1, n, m), INF);
                }
            }
    }

    void addEdge(int from, int to, int speed) {
        Edge edgeA = new Edge(from, to, speed, countEdges + 1);
        Edge edgeB = new Edge(to, from, 0, -(countEdges+ 1));
        edgeA.setBackEdge(edgeB);
        edgeB.setBackEdge(edgeA);
        listEdges.get(from).add(edgeA);
        listEdges.get(to).add(edgeB);
        mapIndexToEdge.put(countEdges + 1, edgeA);
        mapIndexToEdge.put(-(countEdges + 1), edgeB);
        countEdges++;
    }

    int indexOutEdges(int i, int j, int length) {
        return length * i + j;
    }

    int indexInEdges(int i, int j, int height, int length) {
        return height * length + indexOutEdges(i, j, length);
    }
}

class Cell {
    int x, y;
    char ch;
    int vertex1, vertex2;

    Cell (int x, int y, char ch) {
        this.x = x;
        this.y = y;
        this.ch = ch;
    }

    public static boolean isValidCoordinates(int x, int y, int n, int m) {
        return 0 <= x && x < n && 0 <= y && y < m;
    }
}