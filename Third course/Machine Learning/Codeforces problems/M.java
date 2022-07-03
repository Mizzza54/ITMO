import java.io.*;
import java.util.*;


/**
 * @author Michael Gerasimov
 */
public class M {
    public static void main(String[] args) {
        FastReader scanner = new FastReader(System.in);
        PrintWriter writer = new PrintWriter(System.out);

        long k = scanner.nextInt();
        long n = scanner.nextInt();

        List<CustomObject> objects = new ArrayList<>();

        for (int i = 0; i < n; i++) {
            objects.add(new CustomObject(scanner.nextInt(), scanner.nextInt()));
        }

        objects.sort(CustomObject::compareTo);

        long inVal = findIn(objects);
        long outVal = findOut(objects);

        writer.println(inVal);
        writer.println(outVal - inVal);

        writer.flush();
        writer.close();
    }

    private static long findIn(List<CustomObject> objects) {
        Map<Long, List<CustomObject>> map = new HashMap<>();
        for (CustomObject object : objects) {
            if (!map.containsKey(object.y)) {
                map.put(object.y, new ArrayList<>());
            }
            map.get(object.y).add(object);
        }

        List<Long> ans = new ArrayList<>();
        for (Long key: map.keySet()) {
            ans.add(findOut(map.get(key)));
        }

        return ans.stream().mapToLong(x -> x).sum();
    }

    private static long findOut(List<CustomObject> objects) {
        List<Long> difference = new ArrayList<>();
        for (int i = 0; i < objects.size() - 1; i++) {
            difference.add(objects.get(i + 1).x - objects.get(i).x);
        }

        List<Long> sumDifference = new ArrayList<>();
        long acc = 0;
        for (Long aLong : difference) {
            acc += aLong;
            sumDifference.add(acc);
        }

        long a = 0;
        long b = sumDifference.stream().mapToLong(x -> x).sum();
        long ans = b;

        for (int i = 1; i < objects.size(); i++) {
            a += i * (objects.get(i).x - objects.get(i - 1).x);
            b -= (objects.size() - i) * (objects.get(i).x - objects.get(i - 1).x);
            ans += a + b;
        }

        return ans;
    }
}

class CustomObject implements Comparable<CustomObject> {
    long x, y;

    public CustomObject(long x, long y) {
        this.x = x;
        this.y = y;
    }


    @Override
    public int compareTo(CustomObject o) {
        return Long.compare(this.x, o.x);
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
        int result = (int) (x ^ (x >>> 32));
        result = 31 * result + (int) (y ^ (y >>> 32));
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