import java.io.*;
import java.util.Arrays;

/**
 * @author Michael Gerasimov
 * start: 16.04.2021
 * @version -
 */
public class E {
    StreamTokenizer scanner = new StreamTokenizer(new BufferedReader(new InputStreamReader(System.in)));
    PrintWriter writer = new PrintWriter(System.out);

    public static void main(String[] args) {
        E E = new E();
        E.run();
    }

    public void run() {

    }

    int nextInt() {
        try {
            scanner.nextToken();
            return (int) scanner.nval;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return -42;
    }
}

class Polynomial {
    long[] array;
    static final int MAX_ARRAY_SIZE = 102;

    Polynomial(long[] array) {
        int size = Math.min(MAX_ARRAY_SIZE, array.length);
        this.array = new long[MAX_ARRAY_SIZE];
        System.arraycopy(array, 0, this.array, 0, size);
    }

    Polynomial(long number) {
        this.array = new long[]{number};
    }

    public Long getElement(int index) {
        return index >= array.length ? 0L : array[index];
    }

    public int size() {
        return array.length;
    }

    public static Polynomial add(Polynomial x, Polynomial y) {
        long[] result = new long[MAX_ARRAY_SIZE];
        for (int i = 0; i < result.length; i++) {
            result[i] = x.getElement(i) + y.getElement(i);
        }
        return new Polynomial(result);
    }

    public static Polynomial multiply(Polynomial x, Polynomial y) {
        long[] result = new long[MAX_ARRAY_SIZE];
        for (int i = 0; i < MAX_ARRAY_SIZE; i++) {
            for (int j = 0; j < MAX_ARRAY_SIZE - i; j++) {
                result[i + j] += x.getElement(i) * y.getElement(j);
            }
        }
        return new Polynomial(result);
    }

    @Override
    public String toString() {
        return Arrays.toString(array);
    }
}
