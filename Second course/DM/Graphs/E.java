package E;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 07.12.2020
 * @version -
 */
public class E {

    public static int[] Parent;

    public static void main(String[] args) {
        Graph Graph = new Graph();
        Parent = new int[Graph.TheNumberOfVertex + 1];
        Arrays.fill(Parent, -1);
        ArrayList<Integer> Answer = pruferCode(Graph);
        for (int i: Answer) {
            System.out.print(i + " ");
        }
    }

    public static void dfs(int from, Graph Graph) {
        for (int to: Graph.ListOfEdges.get(from)) {
            if (to != Parent[from]) {
                Parent[to] = from;
                dfs(to, Graph);
            }
        }
    }

    public static ArrayList<Integer> pruferCode(Graph Graph) {
        int n = Graph.TheNumberOfVertex + 1;
        dfs (n - 1, Graph);

        int ptr = nextPtr(n, 0, Graph);

        ArrayList<Integer> Answer = new ArrayList<>();
        int leaf = ptr;
        for (int i = 1; i < n - 2; i++) {
            int next = Parent[leaf];
            Answer.add(next);
            Graph.Degree[next]--;
            if (Graph.Degree[next] == 1 && next < ptr) {
                leaf = next;
            } else {
                ptr++;
                ptr = nextPtr(n, ptr, Graph);
                leaf = ptr;
            }
        }
        return Answer;
    }

    public static int nextPtr(int n, int ptr, Graph Graph) {
        while (ptr < n && Graph.Degree[ptr] != 1) {
            ptr++;
        }
        return ptr;
    }
}

class Graph {
    FastReader scanner = new FastReader(System.in);

    int TheNumberOfVertex;
    int TheNumberOfEdges;
    ArrayList<HashSet<Integer>> ListOfEdges;
    int[] Degree;

    Graph () {
        TheNumberOfVertex = scanner.nextInt();
        TheNumberOfEdges = TheNumberOfVertex - 1;
        ListOfEdges = new ArrayList<>(TheNumberOfVertex + 1);
        Degree = new int[TheNumberOfVertex + 1];

        for (int i = 0; i < TheNumberOfVertex + 1; i++) {
            ListOfEdges.add(new HashSet<>());
            Degree[i] = 0;
        }

        for (int i = 0; i < TheNumberOfEdges; i++) {
            int from = scanner.nextInt();
            int to = scanner.nextInt();
            ListOfEdges.get(from).add(to);
            ListOfEdges.get(to).add(from);
            Degree[from]++;
            Degree[to]++;
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