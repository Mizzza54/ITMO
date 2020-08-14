import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Scanner;

/**
 * @author Michale Gerasimov
 * start: 16.12.2019
 * @version -
 */
public class Num2Part {
    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("num2part.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("num2part.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static final int MAX_SIZE = 150;
    public static long[][] sizeOfPartitions;

    public static void main(String[] args) {
        makeSizeOfPart();
        int n;
        long r;
        n = scanner.nextInt();
        r = scanner.nextLong();
        ArrayList<Integer> partitions;
        partitions = num2part(n, r);
        for (int i = 0; i < partitions.size() - 1; i++) {
            writer.print(partitions.get(i) + "+");
        }
        writer.print(partitions.get(partitions.size() - 1));
        writer.close();
    }

    private static ArrayList<Integer> num2part(int n, long r) {
        ArrayList<Integer> partitions = new ArrayList<>();
        int sum = 0;
        int num = 1;
        int limit = n;
        while (sum < limit) {
            if (r < (sizeOfPartitions[n][num] - sizeOfPartitions[n][num + 1])) {
                n -= num;
                sum += num;
                partitions.add(num);
            } else {
                r -= (sizeOfPartitions[n][num] - sizeOfPartitions[n][num + 1]);
                num++;
            }
        }
        return partitions;
    }


    private static void makeSizeOfPart() {
        sizeOfPartitions = new long[MAX_SIZE][MAX_SIZE];
        for (int i = 1; i < MAX_SIZE; i++) {
            for (int j = 1; j < MAX_SIZE; j++) {
                if (i == j) {
                    sizeOfPartitions[i][j] = 1;
                }
                else if (i < j) {
                    sizeOfPartitions[i][j] = 0;
                }
            }
        }
        for (int i = 1; i < MAX_SIZE; i++) {
            for (int j = MAX_SIZE - 1; j >= 1; j--) {
                if (i > j) {
                    sizeOfPartitions[i][j] = 0;
                    sizeOfPartitions[i][j] += sizeOfPartitions[i][j + 1];
                    sizeOfPartitions[i][j] += sizeOfPartitions[i - j][j];
                }
            }
        }
    }
}
