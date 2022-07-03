import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * @author Michael Gerasimov
 */
public class O {
    public static void main(String[] args) {
        FastReader scanner = new FastReader(System.in);
        int k1 = scanner.nextInt();
        int k2 = scanner.nextInt();
        int n = scanner.nextInt();
        List<CustomObject> objects = new ArrayList<>();
        long[] count1 = new long[k1 + 1];
        long[] count2 = new long[k2 + 1];
        for (int i = 0; i < n; i++) {
            objects.add(new CustomObject(scanner.nextInt(), scanner.nextInt()));
            count1[objects.get(i).x]++;
            count2[objects.get(i).y]++;
        }

        Map<CustomObject, Integer> map = new HashMap<>();
        for (int i = 0; i < n; i++) {
            if (!map.containsKey(objects.get(i))) {
                map.put(objects.get(i), 1);
            } else {
                map.put(objects.get(i), map.get(objects.get(i)) + 1);
            }
        }

        double result = 0;
        for (CustomObject key: map.keySet()) {
            int value = map.get(key);
            int numerator = value * value;
            double denominator = count1[key.x] * count2[key.y];
            result += (numerator / denominator);
        }

        System.out.println(n * (result - 1));
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