import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.StringTokenizer;
import java.util.stream.Collectors;

/**
 * @author Michael Gerasimov
 */
public class C {
    public static void main(String[] args) throws IOException {
        FastReader scanner = new FastReader(System.in);
        int n = scanner.nextInt();
        int m = scanner.nextInt();
        List<CustomObject> objects = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            objects.add(new CustomObject(m));
        }

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                objects.get(i).attributes.add(scanner.nextInt());
            }
            objects.get(i).value = scanner.nextInt();
        }

        CustomObject query = new CustomObject(m);
        for (int i = 0; i < m; i++) {
            query.attributes.add(scanner.nextInt());
        }

        Distance distance = Distance.fromString(scanner.next());
        Kernel kernel = Kernel.fromString(scanner.next());
        Window window = Window.fromString(scanner.next());
        int windowParameter = scanner.nextInt();

        for (int i = 0; i < n; i++) {
            objects.get(i).distance = distance.calculate(distance, query.attributes, objects.get(i).attributes);
        }

        objects.sort(Comparator.comparingDouble(o -> o.distance));

        double h = window == Window.VARIABLE
                ? objects.get(windowParameter).distance
                : windowParameter;

        if (h != 0) {
            double[] a = new double[n];
            for (int i = 0; i < n; i++) {
                a[i] = kernel.calculate(kernel, objects.get(i).distance / h);
            }

            double c = 0;
            for (int i = 0; i < n; i++) {
                c += a[i];
            }

            if (c == 0) {
                int sum = 0;
                for (int i = 0; i < n; i++) {
                    sum += objects.get(i).value;
                }
                System.out.println((double) sum / n);
            } else {

                double b = 0;
                for (int i = 0; i < n; i++) {
                    b += a[i] * objects.get(i).value;
                }

                System.out.println(b / c);
            }
        } else {
            if (objects.get(0).attributes.equals(query.attributes)) {
                objects = objects.stream().filter(x -> x.attributes.equals(query.attributes)).collect(Collectors.toList());
            }
            int sum = 0;
            for (CustomObject object : objects) {
                sum += object.value;
            }

            System.out.println((double) sum / objects.size());
        }
    }
}

enum Window {
    FIXED("fixed"),
    VARIABLE("variable");

    private final String value;

    Window(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    public static Window fromString(String value) {
        if (value != null) {
            for (Window name : values()) {
                if (value.equalsIgnoreCase(name.getValue())) {
                    return name;
                }
            }
        }
        return null;
    }
}

enum Kernel {
    UNIFORM("uniform"),
    TRIANGULAR("triangular"),
    EPANECHNIKOV("epanechnikov"),
    QUARTIC("quartic"),
    TRIWEIGHT("triweight"),
    TRICUBE("tricube"),
    GAUSSIAN("gaussian"),
    COSINE("cosine"),
    LOGISTIC("logistic"),
    SIGMOID("sigmoid");

    private final String value;

    Kernel(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    public static Kernel fromString(String value) {
        if (value != null) {
            for (Kernel name : values()) {
                if (value.equalsIgnoreCase(name.getValue())) {
                    return name;
                }
            }
        }
        return null;
    }

    private boolean support(double u) {
        return Math.abs(u) < 1;
    }

    public double calculate(Kernel name, double u) {
        switch (name) {
            case UNIFORM:
                return support(u) ? 0.5 : 0;
            case TRIANGULAR:
                return support(u) ? 1 - Math.abs(u) : 0;
            case EPANECHNIKOV:
                return support(u) ? 0.75 * (1 - u * u) : 0;
            case QUARTIC:
                return support(u) ? ((double) 15 / 16) * ((1 - u * u) * (1 - u * u)) : 0;
            case TRIWEIGHT:
                return support(u) ? ((double) 35 / 32) * Math.pow((1 - u * u), 3) : 0;
            case TRICUBE:
                return support(u) ? ((double) 70 / 81) * Math.pow((1 - Math.pow(Math.abs(u), 3)), 3): 0;
            case GAUSSIAN:
                return Math.exp(-0.5 * u * u) / Math.sqrt(2 * Math.PI);
            case COSINE:
                return support(u) ? (Math.PI / 4) * (Math.cos((Math.PI / 2) * u)) : 0;
            case LOGISTIC:
                return 1 / (Math.exp(u) + 2 + Math.exp(-u));
            case SIGMOID:
                return (2 / Math.PI) * (1 / (Math.exp(u) + Math.exp(-u)));
            default:
                return -1;
        }
    }
}

enum Distance {
    MANHATTAN("manhattan"),
    EUCLIDEAN("euclidean"),
    CHEBYSHEV("chebyshev");

    private final String value;

    Distance(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    public static Distance fromString(String value) {
        if (value != null) {
            for (Distance name : values()) {
                if (value.equalsIgnoreCase(name.getValue())) {
                    return name;
                }
            }
        }
        return null;
    }

    public double calculate(Distance name, List<Integer> a, List<Integer> b) {
        double res = 0;
        switch (name) {
            case MANHATTAN:
                for (int i = 0; i < a.size(); i++) {
                    res += Math.abs(a.get(i) - b.get(i));
                }
                return res;
            case EUCLIDEAN:
                for (int i = 0; i < a.size(); i++) {
                    res += ((a.get(i) - b.get(i)) * (a.get(i) - b.get(i)));
                }
                return Math.sqrt(res);
            case CHEBYSHEV:
                res = Math.abs(a.get(0) - b.get(0));
                for (int i = 1; i < a.size(); i++) {
                    if (Math.abs(a.get(i) - b.get(i)) > res) {
                        res = Math.abs(a.get(i) - b.get(i));
                    }
                }
                return res;
            default:
                return -1;
        }
    }
}

class CustomObject {
    public List<Integer> attributes;
    public int value;
    public double distance;

    public CustomObject(int size) {
        this.attributes = new ArrayList<>();
    }

    @Override
    public String toString() {
        return "CustomObject{" +
                "attributes=" + attributes +
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