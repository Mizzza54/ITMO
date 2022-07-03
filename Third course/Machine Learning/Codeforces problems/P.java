import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * @author Michael Gerasimov
 */
public class P {
    public static void main(String[] args) {
        FastReader scanner = new FastReader(System.in);
        int k1 = scanner.nextInt();
        int k2 = scanner.nextInt();
        int n = scanner.nextInt();
        List<CustomObject> objects = new ArrayList<>();
        Map<Integer, Map<Integer, Long>> map = new HashMap<>();
        int[] count = new int[k1 + 1];
        for (int i = 0; i < n; i++) {
            objects.add(new CustomObject(scanner.nextInt(), scanner.nextInt()));

            if (!map.containsKey(objects.get(i).x)) {
                map.put(objects.get(i).x, new HashMap<>());
                map.get(objects.get(i).x).put(objects.get(i).y, 1L);
            } else {
                if (!map.get(objects.get(i).x).containsKey(objects.get(i).y)) {
                    map.get(objects.get(i).x).put(objects.get(i).y, 1L);
                } else {
                    map.get(objects.get(i).x).put(objects.get(i).y, map.get(objects.get(i).x).get(objects.get(i).y) + 1);
                }
            }

            count[objects.get(i).x]++;
        }

        double result = 0;
        for (Integer key1: map.keySet()) {
            for (Integer key2: map.get(key1).keySet()) {
                long value = map.get(key1).get(key2);
                result += -value / ((double) n) * Math.log(value / (double) count[key1]);
            }
        }

        System.out.println(result);
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