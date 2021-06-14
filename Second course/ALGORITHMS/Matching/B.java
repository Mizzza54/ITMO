import java.io.*;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 06.03.2021
 * @version -
 */
public class B {
    StreamTokenizer scanner = new StreamTokenizer(new BufferedReader(new InputStreamReader(System.in)));
    PrintWriter writer = new PrintWriter(System.out);
    BipartiteGraph graph;
    boolean[] visited;

    public static void main(String[] args) {
        B B = new B();
        B.run();
    }

    public void run() {
        graph = new BipartiteGraph();

        int[] matchingLeft = new int[graph.sizeOfRight];
        visited = new boolean[graph.sizeOfLeft];
        Arrays.fill(matchingLeft, -1);
        for (Pair pair : graph.weightOfLeft) {
            int integer = pair.id;
            Arrays.fill(visited, false);
            dfsKuhn(integer, graph.edgesOfLeft, matchingLeft);
        }

        int[] matchingRight = new int[graph.sizeOfLeft];
        visited = new boolean[graph.sizeOfRight];
        Arrays.fill(matchingRight, -1);
        for (Pair pair : graph.weightOfRight) {
            int integer = pair.id;
            Arrays.fill(visited, false);
            dfsKuhn(integer, graph.edgesOfRight, matchingRight);
        }

        ArrayList<ArrayList<Integer>> newGraph = new ArrayList<>();
        TreeSet<Integer> setLeft = new TreeSet<>();
        TreeSet<Integer> setRight = new TreeSet<>();
        for (int i = 0; i < graph.sizeOfLeft + graph.sizeOfRight; i++) {
            if (i < matchingLeft.length && matchingLeft[i] != -1) {
                setLeft.add(matchingLeft[i]);
            }
            if (i < matchingRight.length && matchingRight[i] != -1) {
                setRight.add(matchingRight[i]);
            }
            newGraph.add(new ArrayList<>());
        }
        for (int i = 0; i < graph.sizeOfLeft; i++) {
            for (int j = 0; j < graph.edgesOfLeft.get(i).size(); j++) {
                if (setLeft.contains(i) && setRight.contains(graph.edgesOfLeft.get(i).get(j))) {
                    newGraph.get(i).add(graph.edgesOfLeft.get(i).get(j));
                }
            }
        }

        int[] matching = new int[graph.sizeOfLeft + graph.sizeOfRight];
        ArrayList<Pair> newWeight = new ArrayList<>();
        Arrays.fill(matching, -1);
        visited = new boolean[graph.sizeOfLeft + graph.sizeOfRight];

        for (int i = 0; i < graph.sizeOfLeft; i++) {
            if (setLeft.contains(i)) {
                newWeight.add(new Pair(i, graph.weightOfLeftReal.get(i)));
            }
        }
        Collections.sort(newWeight);
        for (Pair pair : newWeight) {
            int integer = pair.id;
            Arrays.fill(visited, false);
            dfsKuhn(integer, newGraph, matching);
        }

        int maxWeight = 0;
        ArrayList<Integer> answer = new ArrayList<>();

        for (int i = 0; i < graph.sizeOfRight; i++) {
            if (matching[i] != -1) {
                maxWeight += graph.weightOfLeftReal.get(matching[i]);
                maxWeight += graph.weightOfRightReal.get(i);
                answer.add(graph.indexEdges[matching[i]][i]);
            }
        }

        writer.println(maxWeight);
        writer.println(answer.size());
        for (int i: answer) {
            writer.print(i + " ");
        }
        writer.close();
    }

    public boolean dfsKuhn(int v, ArrayList<ArrayList<Integer>> graph, int[] matching) {
        if (visited[v]) {
            return false;
        }
        visited[v] = true;
        for (int u : graph.get(v)) {
            if (matching[u] == -1 || dfsKuhn(matching[u], graph, matching)) {
                matching[u] = v;
                return true;
            }
        }
        return false;
    }

    class BipartiteGraph {
        int sizeOfLeft, sizeOfRight, countOfEdges;
        ArrayList<Pair> weightOfLeft = new ArrayList<>();
        ArrayList<Integer> weightOfLeftReal = new ArrayList<>();
        ArrayList<Pair> weightOfRight = new ArrayList<>();
        ArrayList<Integer> weightOfRightReal = new ArrayList<>();
        ArrayList<ArrayList<Integer>> edgesOfLeft = new ArrayList<>();
        ArrayList<ArrayList<Integer>> edgesOfRight = new ArrayList<>();
        int[][] indexEdges;

        BipartiteGraph() {
            sizeOfLeft = nextInt();
            sizeOfRight = nextInt();
            countOfEdges = nextInt();

            for (int i = 0; i < sizeOfLeft; i++) {
                int x = nextInt();
                edgesOfLeft.add(new ArrayList<>());
                weightOfLeft.add(new Pair(i, x));
                weightOfLeftReal.add(x);
            }
            for (int i = 0; i < sizeOfRight; i++) {
                int x = nextInt();
                edgesOfRight.add(new ArrayList<>());
                weightOfRight.add(new Pair(i, x));
                weightOfRightReal.add(x);
            }

            indexEdges = new int[sizeOfLeft][sizeOfRight];
            for (int i = 0; i < countOfEdges; i++) {
                int left, right;
                left = nextInt() - 1;
                right = nextInt() - 1;
                edgesOfLeft.get(left).add(right);
                edgesOfRight.get(right).add(left);
                indexEdges[left][right] = i + 1;
            }

            Collections.sort(weightOfLeft);
            Collections.sort(weightOfRight);
        }
    }

    class Pair implements Comparable<Pair>, Comparator<Pair> {
        int id;
        int weight;

        Pair(int id, int weight) {
            this.weight = weight;
            this.id = id;
        }

        @Override
        public int compareTo(Pair o) {
            return Integer.compare(o.weight, this.weight);
        }

        @Override
        public int compare(Pair o1, Pair o2) {
            return Integer.compare(o2.weight, o1.weight);
        }

        @Override
        public String toString() {
            return "Pair{" + "id=" + id + ", weight=" + weight + '}';
        }
    }

    int nextInt() {
        try {
            scanner.nextToken();
            return (int) scanner.nval;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return -42;
    }
}
