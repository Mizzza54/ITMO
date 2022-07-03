import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * @author Michael Gerasimov
 */
public class A {

    public static void main(String[] args) {
        FastReader fastReader = new FastReader(System.in);
        int n, m, k;
        n = fastReader.nextInt();
        m = fastReader.nextInt();
        k = fastReader.nextInt();

        List<Queue<Integer>> classToObjs = new ArrayList<>();
        for (int i = 0; i < m; i++) {
            classToObjs.add(new ArrayDeque<>());
        }

        for (int i = 0; i < n; i++) {
            classToObjs.get(fastReader.nextInt() - 1).add(i + 1);
        }

        List<List<Integer>> result = new ArrayList<>();
        for (int i = 0; i < k; i++) {
            result.add(new ArrayList<>());
        }

        for (int i = 0, j = 0; i < m; i++) {
            while (!classToObjs.get(i).isEmpty()) {
                result.get(j).add(classToObjs.get(i).poll());
                j = (j + 1) % k;
            }
        }

        for (int i = 0; i < k; i++) {
            System.out.print(result.get(i).size() + " ");
            for (int num : result.get(i)) {
                System.out.print(num + " ");
            }
            System.out.println();
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
