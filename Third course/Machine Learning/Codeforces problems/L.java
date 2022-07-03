import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * @author Michael Gerasimov
 */
public class L {
    public static void main(String[] args) {
        FastReader scanner = new FastReader(System.in);
        int n = scanner.nextInt();

        List<Integer> objects1 = new ArrayList<>();
        List<Integer> objects2 = new ArrayList<>();
        Map<Integer, Integer> map1, map2;
        for (int i = 0; i < n; i++) {
            objects1.add(scanner.nextInt());
            objects2.add(scanner.nextInt());
        }

        List<Integer> sortedObjects1 = objects1.stream().sorted().collect(Collectors.toList());
        List<Integer> sortedObjects2 = objects2.stream().sorted().collect(Collectors.toList());
        map1 = IntStream.range(0, sortedObjects1.size()).boxed().collect(Collectors.toMap(sortedObjects1::get, i -> i));
        map2 = IntStream.range(0, sortedObjects2.size()).boxed().collect(Collectors.toMap(sortedObjects2::get, i -> i));

        double numerator = 0;
        for (int i = 0; i < n; i++) {
            numerator += Math.pow(map1.get(objects1.get(i)) - map2.get(objects2.get(i)), 2);
        }

        double denominator = Math.pow(n, 3) - n;

        System.out.println(1 - 6 * (numerator / denominator));
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