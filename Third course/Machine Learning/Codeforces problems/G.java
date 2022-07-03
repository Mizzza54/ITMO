import java.io.*;
import java.util.Arrays;
import java.util.StringTokenizer;
import java.util.stream.IntStream;


/**
 * @author Michael Gerasimov
 */
public class G {
    public static void main(String[] args) {
        FastReader scanner = new FastReader(System.in);
        PrintWriter writer = new PrintWriter(System.out);

        int m = scanner.nextInt();
        int[] truthTable = new int[1 << m];
        int countZero = 0;
        int countOne = 0;

        for (int i = 0; i < 1 << m; i++) {
            truthTable[i] = scanner.nextInt();
            if (truthTable[i] == 0) {
                countZero++;
            } else {
                countOne++;
            }
        }

        if (countZero == truthTable.length || countOne == truthTable.length) {
            writer.println(1);
            writer.println(1);
            for (int i = 0; i < m; i++) {
                writer.print("0 ");
            }

            writer.print(countZero == truthTable.length ? "-0.5" : "0.5");
        } else {
            writer.println(2);

            if (countZero >= countOne) {
                writer.println(countOne + " 1");
                int[][] pdnf = findPDNF(truthTable, m);

                for (int[] ints : pdnf) {
                    double count = 0.5;
                    for (int j = 0; j < pdnf[0].length; j++) {
                        writer.print(ints[j] + " ");
                        count = ints[j] == 1 ? count - 1 : count;
                    }
                    writer.println(count);
                }

                for (int i = 0; i < countOne; i++) {
                    writer.print("1 ");
                }
                writer.println("-0.5");
            } else {
                writer.println(countZero + " 1");
                int[][] pcnf = findPCNF(truthTable, m);

                for (int[] ints : pcnf) {
                    double count = -0.5;
                    for (int j = 0; j < pcnf[0].length; j++) {
                        writer.print(ints[j] + " ");
                        count = ints[j] == -1 ? count + 1 : count;
                    }
                    writer.println(count);
                }

                for (int i = 0; i < countZero; i++) {
                    writer.print("1 ");
                }
                writer.println(0.5 - countZero);
            }
        }


        writer.flush();
        writer.close();
    }

    public static int[][] findPDNF(int[] truthTable, int countArgs) {
        int countOne = (int) Arrays.stream(truthTable).filter(i -> i == 1).count();
        int[][] PDNF = new int[countOne][countArgs];

        for (int k = 0, i = 0; k < truthTable.length; k++) {
            if (truthTable[k] == 1) {
                int[] bitVector = indexToArgs(k, countArgs);
                for (int j = 0; j < bitVector.length; j++) {
                    PDNF[i][j] = bitVector[j] == 0 ? -1 : 1;
                }
                i++;
            }
        }
        return PDNF;
    }

    public static int[][] findPCNF(int[] truthTable, int countArgs) {
        int countZero = (int) Arrays.stream(truthTable).filter(i -> i == 0).count();
        int[][] PCNF = new int[countZero][countArgs];

        for (int k = 0, i = 0; k < truthTable.length; k++) {
            if (truthTable[k] == 0) {
                int[] bitVector = indexToArgs(k, countArgs);
                for (int j = 0; j < bitVector.length; j++) {
                    PCNF[i][j] = bitVector[j] == 0 ? 1 : -1;
                }
                i++;
            }
        }
        return PCNF;
    }

    private static int[] indexToArgs(int index, int countArgs) {
        int[] bitVector = new int[countArgs];
        for (int i = 0; i < countArgs; i++) {
            int temp = 1 << (countArgs - i - 1);
            if (index >= temp) {
                index -= temp;
                bitVector[i] = 1;
            } else {
                bitVector[i] = 0;
            }
        }
        return IntStream
                .rangeClosed(1, bitVector.length)
                .map(i -> bitVector[bitVector.length - i])
                .toArray();
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