
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Deque;
import java.util.Scanner;

/**
 * @author Michael Gerasimov
 * start: 15.04.2021
 * @version -
 */
public class G {
    public static void main(String[] args) {
        G G = new G();
        G.run();
    }

    public void run() {
        Scanner scanner = new Scanner(System.in);
        String string = scanner.nextLine();
        string = new StringBuilder(string).reverse().toString();

        Deque<long[]> objects = new ArrayDeque<>();
        long[] object;

        for (int i = 0; i < string.length(); i++) {
            switch (string.charAt(i)) {
                case 'B':
                    object = new long[7];
                    object[1] = 1;
                    objects.add(object);
                    break;
                case 'L':
                    object = objects.pollLast();
                    objects.add(buildSeq(object));
                    break;
                case 'S':
                    object = objects.pollLast();
                    objects.add(buildMSet(object));
                    break;
                case 'P':
                    long[] objectLeft = objects.pollLast();
                    long[] objectRight = objects.pollLast();
                    objects.add(buildPair(objectLeft, objectRight));
                    break;
            }
            //debugPrint(string, i, objects);
        }

        long[] answer = objects.pollLast();
        for (long l : answer) {
            System.out.print(l + " ");
        }
    }

    public void debugPrint(String string, int i, Deque<long[]> objects) {
        System.out.println("Current token = " + string.charAt(i));
        for (long[] array: objects) {
            System.out.println(Arrays.toString(array));
        }
        System.out.println();
    }

    public long[] buildSeq(long[] weight) {
        long[] result = new long[7];

        result[0] = 1;
        for (int i = 1; i < 7; i++) {
            for (int j = 1; j <= i; j++) {
                result[i] += weight[j] * result[i - j];
            }
        }

        return result;
    }

    long factorial(long n) {
        long result = 1;
        for (long i = 1; i <= n; i++) {
            result *= i;
        }
        return result;
    }

    public long combinationsWithRepeat(long n, long k) {
        if (k == 0 || k == n + k - 1) {
            return 1;
        }

        long numerator = n;
        for (long i = n + 1; i <= n + k - 1; i++) {
            numerator *= i;
        }

        return numerator / factorial(k);
    }

    public long[] buildMSet(long[] weight) {
        long[] result = new long[7];
        long[][] m = new long[7][7];

        for (int i = 0; i < 7; i++) {
            m[i][0] = 0;
            m[0][i] = 1;
        }

        for (int i = 1; i < 7; i++) {
            for (int j = 1; j < 7; j++) {
                if (weight[j] == 0) {
                    m[i][j] = m[i][j - 1];
                    continue;
                }
                for (int k = 0; k <= i / j; k++) {
                    m[i][j] += combinationsWithRepeat(weight[j], k) * m[i - k * j][j - 1];
                }
            }
        }

        for (int i = 0; i < 7; i++) {
            result[i] = m[i][i];
        }

        return result;
    }


    public long[] buildPair(long[] leftWeight, long[] rightWeight) {
        long[] result = new long[7];
        for (int i = 0; i < 7; i++) {
            for (int j = 0; j < 7 - i; j++) {
                result[i + j] += leftWeight[i] * rightWeight[j];
            }
        }

        return result;
    }
}
