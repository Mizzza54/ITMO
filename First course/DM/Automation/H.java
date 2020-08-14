import java.io.*;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 11.05.2020
 * @version -
 */

public class H {
    public static FastScanner scanner;
    public static PrintWriter writer;

    public static HashSet<Integer> terminals = new HashSet<>();
    public static HashSet<Integer> notTerminals = new HashSet<>();
    public static ArrayList<HashSet<Integer>> states = new ArrayList<>();
    public static ArrayList<HashSet<Integer>> Partition = new ArrayList<>();
    public static Queue<Pair<Integer, Integer>> queue = new LinkedList<>();
    public static int[][] Transfer;
    public static ArrayList<Integer>[][] Inv;
    public static boolean[] isFinalStates;

    private final static int alphabet = 26;

    public static void main(String[] arg) throws FileNotFoundException {
        scanner = new FastScanner(new File("minimization.in"));
        writer = new PrintWriter(new File("minimization.out"));

        int n = scanner.nextInt() + 1, m = scanner.nextInt(), k = scanner.nextInt();
        isFinalStates = new boolean[n];
        Transfer = new int[n + 1][alphabet];
        Inv = new ArrayList[n + 1][alphabet];

        for (int i = 0; i < n; i++) {
            Inv[i] = new ArrayList[alphabet];
            for (int c = 0; c < alphabet; c++) {
                Transfer[i][c] = 0;
                Inv[i][c] = new ArrayList<>();
            }
        }

        boolean[] reachable = new boolean[n];
        for (int i = 0; i < k; i++) {
            int tmp = scanner.nextInt();
            terminals.add(tmp);
            isFinalStates[tmp] = true;
        }

        for (int i = 0; i < n; i++) {
            if (!isFinalStates[i]) {
                notTerminals.add(i);
            }
        }

        for (int i = 0; i < m; i++) {
            int a = scanner.nextInt();
            int b = scanner.nextInt();
            int ch = scanner.next().charAt(0) - 'a';
            Transfer[a][ch] = b;
        }


        for (int i = 0; i < n; i++) {
            for (int c = 0; c < alphabet; c++) {
                Inv[Transfer[i][c]][c].add(i);
            }
        }



        doReachable(1, Transfer, reachable);
        int[] cs = new int[n + 1];
        Arrays.fill(cs, -1);

        find(n);

        int classes = 0;
        for (HashSet<Integer> state : states) {
            if (state.contains(0)) {
                for (int v : state) {
                    cs[v] = 0;
                }
            }
            if (state.contains(1) && cs[1] == -1) {
                classes++;
                for (int v : state) {
                    cs[v] = 1;
                }
            }
        }
        for (HashSet<Integer> state : states) {
            int v = state.iterator().next();
            if (!reachable[v] || cs[v] != -1) {
                continue;
            }
            classes++;
            cs[v] = classes;
            for (int vertex : state) {
                cs[vertex] = classes;
            }
        }
        boolean[] newTerminal = new boolean[classes + 1];
        int newK = 0;

        for (int i = 0; i < n; i++) {
            if (isFinalStates[i] && cs[i] != -1 && !newTerminal[cs[i]]) {
                newTerminal[cs[i]] = true;
                newK++;
            }
        }
        if (newTerminal[0]) {
            newK--;
        }
        int[][] newTransfer = new int[classes + 1][alphabet];
        for (int i = 0; i < Transfer.length; i++) {
            for (int j = 0; j < Transfer[i].length; j++) {
            }
        }
        int newM = 0;
        for (int i = 0; i < n; i++) {
            for (int c = 0; c < alphabet; c++) {
                int j = Transfer[i][c];
                if (cs[i] > 0 && cs[j] > 0 && newTransfer[cs[i]][c] == 0) {
                    newTransfer[cs[i]][c] = cs[j];
                    newM++;
                }
            }
        }

        writer.println(classes + " " + newM + " " + newK);
        for (int i = 1; i <= classes; i++) {
            if (newTerminal[i]) {
                writer.print(i + " ");
            }
        }
        writer.println();
        for (int i = 1; i <= classes; i++) {
            for (int c = 0; c < alphabet; c++) {
                if (newTransfer[i][c] != 0) {
                    String s = "";
                    s += (char)('a' + c);
                    writer.println(i + " " + newTransfer[i][c] + " " + s);
                }
            }
        }

        writer.close();
    }

    static void doReachable(int v, int[][] Transfer, boolean[] reachable) {
        reachable[v] = true;
        for (int c = 0; c < alphabet; c++) {
            if (!reachable[Transfer[v][c]]) {
                doReachable(Transfer[v][c], Transfer, reachable);
            }
        }
    }

    public static void find(int n) {
        int[] Class = new int[n + 1];
        for (int i = 0; i < n; i++) {
            if (isFinalStates[i]) {
                Class[i] = 0;
            } else {
                Class[i] = 1;
            }
        }

        states.add(terminals);
        states.add(notTerminals);
        Partition.add(new HashSet<>(terminals));
        Partition.add(new HashSet<>(notTerminals));

        for (int c = 0; c < alphabet; c++) {
            queue.add(new Pair<>(0, c));
            queue.add(new Pair<>(1, c));
        }

        while (!queue.isEmpty()) {
            Pair<Integer, Integer> current = queue.poll();
            HashMap<Integer, ArrayList<Integer>> Involved = new HashMap<>();
            for (int q : Partition.get(current.first)) {
                for (int r : Inv[q][current.second]) {
                    if (!Involved.containsKey(Class[r])) {
                        Involved.put(Class[r], new ArrayList<>());
                    }
                    Involved.get(Class[r]).add(r);
                }
            }

            for (int i: Involved.keySet()) {

                if (Involved.get(i).size() < getSize(states.get(i))) {
                    states.add(new HashSet<>());
                    int j = states.size() - 1;
                    for (int rev : Involved.get(i)) {
                        states.get(i).remove(rev);
                        states.get(j).add(rev);
                    }
                    if (states.get(j).size() > states.get(i).size()) {
                        HashSet<Integer> tmp = states.get(j);
                        states.set(j, states.get(i));
                        states.set(i, tmp);
                    }
                    for (int r : states.get(j)) {
                        Class[r] = j;
                    }
                    Partition.add(new HashSet<>(states.get(j)));
                    for (int c = 0; c < alphabet; c++) {
                        queue.add(new Pair<>(Partition.size() - 1, c));
                    }
                }
            }
        }
    }

    public static int getSize(HashSet<Integer> hs) {
        if (hs == null) {
            return 0;
        }
        return hs.size();
    }

    static class Pair<F, S> {
        F first;
        S second;

        Pair(F first, S second) {
            this.first = first;
            this.second = second;
        }
    }

    static class FastScanner {
        BufferedReader br;
        StringTokenizer st;

        FastScanner(File f) {
            try {
                br = new BufferedReader(new FileReader(f));
            } catch (FileNotFoundException e) {
                e.printStackTrace();
            }
        }

        String next() {
            while (st == null || !st.hasMoreTokens()) {
                try {
                    st = new StringTokenizer(br.readLine());
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
            return st.nextToken();
        }

        int nextInt() {
            return Integer.parseInt(next());
        }
    }
}