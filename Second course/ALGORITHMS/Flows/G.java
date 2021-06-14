import java.io.*;
import java.util.*;
import java.util.stream.IntStream;

public class G {
    PrintWriter writer = new PrintWriter(System.out);

    long INF = Long.MAX_VALUE;
    boolean[] reached;
    long[] potentials;
    long[] distance;

    public static void main(String[] args) {
        G G = new G();
        G.run();

    }

    public void run() {
        Graph graph = new Graph();
        System.out.println(findMinCostMaxFlow(1, graph.numberVertex, graph));
        writer.flush();
        writer.close();
    }

    public long findMinCostMaxFlow(int s, int t, Graph graph) {
        reached = new boolean[graph.numberVertex + 1];
        distance = new long[graph.numberVertex + 1];
        potentials = new long[graph.numberVertex + 1];
        Edge[] path;
        long result = 0;

        while (true) {
            path = dijkstra(s, t, graph);
            if (!reached[graph.numberVertex]) {
                break;
            }

            potentials = IntStream.range(0, graph.numberVertex + 1)
                    .mapToLong(v -> reached[v] ? potentials[v] + distance[v] : 0L)
                    .toArray();

            long maxFlow = INF;
            for (int index = graph.numberVertex; index != 1; index = path[index].from) {
                Edge edge = path[index];
                maxFlow = Math.min(maxFlow, edge.speed - edge.flow);
            }

            for (int index = graph.numberVertex; index != 1; index = path[index].from) {
                Edge edge = path[index];
                edge.setFlow(edge.getFlow() + maxFlow);
                edge.getBackEdge().setFlow(edge.getBackEdge().getFlow() - maxFlow);
                result += maxFlow * edge.price;
            }

        }

        return result;
    }

    public Edge[] dijkstra(int start, int finish, Graph graph) {
        distance = new long[graph.numberVertex + 1];
        Edge[] parent = new Edge[graph.numberVertex + 1];
        Arrays.fill(distance, Integer.MAX_VALUE);
        Arrays.fill(reached, false);
        distance[start] = 0;

        Queue<Edge> PriorityQueue = new PriorityQueue<>();
        PriorityQueue.add(new Edge(1, 1, 0));

        while (!PriorityQueue.isEmpty()) {
            Edge curEdge = PriorityQueue.poll();
            if (distance[curEdge.to] < curEdge.price) {
                continue;
            }

            reached[curEdge.to] = true;

            for (Edge e: graph.listEdges.get(curEdge.to)) {
                if (e.getSpeed() - e.getFlow() > 0 && !reached[e.to]) {
                    if (distance[e.to] > distance[e.from] + e.price + potentials[e.from] - potentials[e.to]) {
                        distance[e.to] = distance[e.from] + e.price + potentials[e.from] - potentials[e.to];
                        PriorityQueue.add(new Edge(curEdge.to, e.to, distance[e.to]));
                        parent[e.to] = e;
                    }
                }
            }
        }
        //System.out.println(Arrays.toString(reached));
        return parent;
    }
}

class Edge implements Comparable<Edge> {
    int from, to;
    long speed, index, flow, price;
    Edge backEdge;

    public Edge(int from, int to, long speed, long price, long index) {
        this.from = from;
        this.to = to;
        this.speed = speed;
        this.index = index;
        this.flow = 0;
        this.price = price;
    }

    public Edge(int from, int to, long price) {
        this.from = from;
        this.to = to;
        this.price = price;
    }

    public long getFrom() {
        return from;
    }

    public long getTo() {
        return to;
    }

    public void setFlow(long flow) {
        this.flow = flow;
    }

    public long getFlow() {
        return flow;
    }

    public long getSpeed() {
        return speed;
    }

    public void setBackEdge(Edge backEdge) {
        this.backEdge = backEdge;
    }

    public Edge getBackEdge() {
        return backEdge;
    }

    public long getIndex() {
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
                ", price=" + price +
                '}';
    }

    @Override
    public int compareTo(Edge o) {
        return Long.compare(this.price, o.price);
    }

    public String toStringAnswer() {
        return Long.toString(flow);
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
            int price = scanner.nextInt();
            Edge edgeA = new Edge(from, to, speed, price, i + 1);
            Edge edgeB = new Edge(to, from, 0, -price, -(i + 1));
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