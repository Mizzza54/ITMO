package F;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 07.12.2020
 * @version -
 */
public class F {

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int n = Integer.parseInt(scanner.nextLine());
        ArrayList<Integer> PruferCode = new ArrayList<>();
        String[] string = scanner.nextLine().split(" ");
        for (int i = 0; i < n - 2; i++) {
            PruferCode.add(Integer.parseInt(string[i]));
        }
        ArrayList<Edge> Answer = pruferDecode(PruferCode);
        for (Edge uv: Answer) {
            System.out.println((uv.u + 1) + " " + (uv.v + 1));
        }
    }

    public static ArrayList<Edge> pruferDecode(ArrayList<Integer> PruferCode) {
        int n = PruferCode.size() + 2;
        int[] Degree = new int[n];
        Arrays.fill(Degree, 1);
        for (int i = 0; i < n - 2; i++) {
            Degree[PruferCode.get(i) - 1]++;
        }

        int ptr = nextPtr(n, 0, Degree);
        int leaf = ptr;

        ArrayList<Edge> Answer = new ArrayList<>();
        for (int i = 0; i < n - 2; i++) {
            int v = PruferCode.get(i) - 1;
            Answer.add(new Edge(leaf, v));

            Degree[leaf]--;
            Degree[v]--;
            if (Degree[v] == 1 && v < ptr) {
                leaf = v;
            }
            else {
                ptr++;
                ptr = nextPtr(n, ptr, Degree);
                leaf = ptr;
            }
        }
        for (int v = 0; v < n - 1; v++)
            if (Degree[v] == 1) {
                Answer.add(new Edge(v, n - 1));
            }
        return Answer;
    }

    public static int nextPtr(int n, int ptr, int[] Degree) {
        while (ptr < n && Degree[ptr] != 1) {
            ptr++;
        }
        return ptr;
    }
}

class Edge {
    int v;
    int u;

    Edge(int v, int u) {
        this.v = v;
        this.u = u;
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