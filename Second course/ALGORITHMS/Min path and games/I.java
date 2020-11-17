package I;

import java.io.*;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 13.11.2020
 * @version -
 */
public class I {
    public static Scanner scanner = new Scanner(System.in);

    public static void main(String[] args) {
        StringBuilder Answer = new StringBuilder();

        while (scanner.hasNext()) {
            int n = scanner.nextInt();
            int m = scanner.nextInt();

            if (n == -1) {
                break;
            }

            Graph Graph = new Graph(n, m);
            for (int i = 0; i < Graph.TheNumberOfEdges; i++) {
                int from = scanner.nextInt();
                int to = scanner.nextInt();
                Graph.ListOfEdgesForEachVertexInvert.get(to).add(new Edge(to, from));
                Graph.DegreeOutForEachVertex.set(from, Graph.DegreeOutForEachVertex.get(from) + 1);
            }

            Queue<Integer> Queue = new LinkedList<>();
            for (int v: Graph.Vertex) {
                if (Graph.DegreeOutForEachVertex.get(v) == 0) {
                    Queue.add(v);
                    Graph.StateOfVertex.set(v, State.LOSE);
                }
            }

            while (!Queue.isEmpty()) {
                int v = Queue.poll();
                for (Edge e: Graph.ListOfEdgesForEachVertexInvert.get(v)) {
                    int u = e.to;
                    if (Graph.StateOfVertex.get(u) == State.DRAW) {
                        if (Graph.StateOfVertex.get(v) == State.WIN) {
                            Graph.DegreeOutForEachVertex.set(u, Graph.DegreeOutForEachVertex.get(u) - 1);
                        }

                        if (Graph.StateOfVertex.get(v) == State.LOSE) {
                            Graph.StateOfVertex.set(u, State.WIN);
                        } else if (Graph.StateOfVertex.get(v) == State.WIN && Graph.DegreeOutForEachVertex.get(u) == 0) {
                            Graph.StateOfVertex.set(u, State.LOSE);
                        } else {
                            continue;
                        }
                        Queue.add(u);
                    }
                }
            }

            for (int i = 1; i < Graph.StateOfVertex.size(); i++) {
                switch (Graph.StateOfVertex.get(i)) {
                    case LOSE:
                        Answer.append("SECOND\n");
                        break;
                    case WIN:
                        Answer.append("FIRST\n");
                        break;
                    case DRAW:
                        Answer.append("DRAW\n");
                        break;
                }
            }
            Answer.append("\n");
        }
        System.out.println(Answer.toString());
    }
}

enum State {
    WIN, LOSE, DRAW;

    @Override
    public String toString() {
        switch(this) {
            case WIN: return "WIN";
            case LOSE: return "LOSE";
            case DRAW: return "DRAW";
            default: throw new IllegalArgumentException();
        }
    }
}

class Graph {

    int TheNumberOfVertex;
    int TheNumberOfEdges;
    TreeSet<Integer> Vertex = new TreeSet<>();
    ArrayList<ArrayList<Edge>> ListOfEdgesForEachVertexInvert = new ArrayList<>();
    ArrayList<Integer> DegreeOutForEachVertex = new ArrayList<>();
    ArrayList<State> StateOfVertex = new ArrayList<>();


    public Graph(int n, int m) {
        TheNumberOfVertex = n;
        TheNumberOfEdges = m;
        for (int i = 0; i < TheNumberOfVertex + 1; i++) {
            if (i != 0) {
                Vertex.add(i);
            }
            StateOfVertex.add(State.DRAW);
            DegreeOutForEachVertex.add(0);
            ListOfEdgesForEachVertexInvert.add(new ArrayList<Edge>());
        }
    }
}

class Edge {
    Integer from;
    Integer to;

    Edge(Integer left, Integer right) {
        this.from = left;
        this.to = right;
    }
}