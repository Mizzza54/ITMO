import java.io.*;
import java.util.*;
import java.util.stream.Collectors;


/**
 * @author Michael Gerasimov
 */
public class N {
    public static void main(String[] args) {
        FastReader scanner = new FastReader(System.in);
        PrintWriter writer = new PrintWriter(System.out);

        int k = scanner.nextInt();
        int n = scanner.nextInt();
        List<CustomObject> objects = new ArrayList<>();
        Map<Integer, Double> map1 = new HashMap<>();
        Map<Integer, List<Integer>> map2 = new HashMap<>();

        for (int i = 0; i < n; i++) {
            objects.add(new CustomObject(scanner.nextInt(), scanner.nextInt()));
            map1.put(objects.get(i).x, map1.getOrDefault(objects.get(i).x, 0.0) + 1);

            if (!map2.containsKey(objects.get(i).x)) {
                map2.put(objects.get(i).x, new ArrayList<>());
            }
            map2.get(objects.get(i).x).add(objects.get(i).y);
        }

        map1.replaceAll((k1, v) -> map1.get(k1) / n);

        List<Double> array = new ArrayList<>();
        for (Integer key: map2.keySet()) {

            List<Integer> tempArray = map2.get(key).stream().map(x -> (int) Math.pow(x, 2)).collect(Collectors.toList());
            double temp = evalE(tempArray, map1.size(), (double) map2.get(key).size() / map1.size()) -
                    Math.pow(evalE(map2.get(key), map1.size(), (double) map2.get(key).size() / map1.size()), 2);
            double temp2 = map1.get(key) * temp;
            array.add(temp2);
        }

        writer.println(array.stream().mapToDouble(x -> x).sum());

        writer.flush();
        writer.close();
    }

    private static double evalE(List<Integer> array, int size, double p) {
        Map<Integer, Integer> map = new HashMap<>();
        for (Integer y : array) {
            map.put(y, map.getOrDefault(y, 0) + 1);
        }

        List<Double> result = new ArrayList<>();
        for (Integer key: map.keySet()) {
            result.add(key * (map.get(key) / (double) size) / p);
        }

        return result.stream().mapToDouble(a -> a).sum();
    }
}

class CustomObject implements Comparable<CustomObject> {
    int x, y;

    public CustomObject(int x, int y) {
        this.x = x;
        this.y = y;
    }


    @Override
    public int compareTo(CustomObject o) {
        return Integer.compare(this.x, o.x);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CustomObject that = (CustomObject) o;

        if (x != that.x) return false;
        return y == that.y;
    }

    @Override
    public int hashCode() {
        int result = x;
        result = 31 * result + y;
        return result;
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