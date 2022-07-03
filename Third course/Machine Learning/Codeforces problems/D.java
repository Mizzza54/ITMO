import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * @author Michael Gerasimov
 */
public class D {

    public static void main(String[] args) {
        FastReader scanner = new FastReader(System.in);
        int n = scanner.nextInt();
        int m = scanner.nextInt();

        List<CustomObject> objects = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            objects.add(new CustomObject(m));
        }

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                objects.get(i).attributes[j] = scanner.nextInt();
            }
            objects.get(i).value = scanner.nextInt();
        }

        int[] w = new int[m + 1];
        for (int i = 0; i < m + 1; i++) {
            w[i] = scanner.nextInt();
        }

        for (int i = 0; i < n; i++) {
            for (Double j : gradient(objects.get(i), w)) {
                System.out.print(j + " ");
            }
            System.out.println();
        }

    }

    private static List<Double> gradient(CustomObject x, int[] w) {
        List<Double> gradient = new ArrayList<>();
        double scalar = 0;


        for (int i = 0; i < x.attributes.length; i++) {
            scalar += x.attributes[i] * w[i];
        }

        scalar += w[w.length - 1];

        for (int i = 0; i < x.attributes.length; i++) {
            double numerator = Math.abs(scalar - x.value);
            double denominator = Math.abs(scalar) + Math.abs(x.value);
            double temp1 = numerator == 0 ? 0 : (-numerator / (x.value - scalar)) * x.attributes[i];
            double temp2 = scalar == 0 ? 0 : (Math.abs(scalar) / scalar) * x.attributes[i];
            gradient.add((temp1 * denominator - numerator * temp2) / (denominator * denominator));
        }

        double numerator = Math.abs(scalar - x.value);
        double denominator = Math.abs(scalar) + Math.abs(x.value);
        double temp1 = numerator == 0 ? 0 : (-numerator / (x.value - scalar)) * 1;
        double temp2 = scalar == 0 ? 0 : (Math.abs(scalar) / scalar) * 1;
        gradient.add((temp1 * denominator - numerator * temp2) / (denominator * denominator));

        return gradient;
    }
}

class CustomObject {
    public int[] attributes;
    public int value;

    public CustomObject(int size) {
        this.attributes = new int[size];
    }

    @Override
    public String toString() {
        return "CustomObject{" +
                "attributes=" + Arrays.toString(attributes) +
                ", value=" + value +
                '}';
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