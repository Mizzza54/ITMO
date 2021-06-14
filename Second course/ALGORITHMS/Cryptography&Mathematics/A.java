import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 25.05.2021
 * @version -
 */
public class A {
    PrintWriter writer = new PrintWriter(System.out);
    FastReader scanner = new FastReader(System.in);
    private static int INF = 1000000000;

    public static void main(String[] args) {
        A A = new A();
        A.run();
    }

    public void run() {
        int number = scanner.nextInt();
        for (int i: getMultipliers(number)) {
            writer.write(Integer.toString(i));
            writer.write(" ");
        }
        writer.flush();
        writer.close();
    }

    private List<Integer> getMultipliers(int number) {
        List<Integer> result = new ArrayList<>();
        int probe = 2;
        while (number != 1) {
            if (number % probe != 0) {
                probe++;
            } else {
                number /= probe;
                result.add(probe);
            }
        }
        result.sort(Comparator.naturalOrder());
        return result;
    }

//    private List<Integer> getMultipliers(int number) {
//        List<Integer> result = new ArrayList<>();
//        int[] sieve = sieveOfEratosthenes(number);
//        while (number != 1) {
//            if (sieve[number] == 1) {
//                sieve[number] = number;
//            }
//            result.add(sieve[number]);
//            number /= sieve[number];
//        }
//        result.sort(Comparator.naturalOrder());
//        return result;
//    }

    private int[] sieveOfEratosthenes(int n) {
        int[] result = new int[n + 1];
        Arrays.fill(result, 1);
        result[n] = n;
        for (int i = 2; i <= Math.round(Math.floor(Math.sqrt(n))); i++) {
            if (result[i] == 1) {
                result[i] = i;
                int shuttle = i * i;
                while (shuttle <= n) {
                    result[shuttle] = i;
                    shuttle += i;
                }
            }
        }
        return result;
    }

    private List<Integer> getDividers(int number) {
        List<Integer> result = new ArrayList<>();
        for (int probe = 2; probe <= Math.round(Math.floor(Math.sqrt(number))); probe++) {
            if (number % probe == 0){
                result.add(probe);
                result.add(number / probe);
            }
        }
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