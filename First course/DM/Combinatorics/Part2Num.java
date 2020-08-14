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
public class Part2Num {
    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("part2num.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("part2num.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static final int MAX_SIZE = 101;
    public static long[][] sizeOfPartitions;

    public static void main(String[] args) {
        makeSizeOfPart();
        String[] line = scanner.nextLine().split("[+]");
        int number = 0;
        ArrayList<Integer> partition = new ArrayList<Integer>();
        for (int i = 0; i < line.length; i++) {
            partition.add(Integer.parseInt(line[i]));
            number += Integer.parseInt(line[i]);
        }
        writer.print(partition2num(partition, number));
        writer.close();
    }

    public static long partition2num (ArrayList<Integer> partition, int number) {
        long numOfPartition = 0;
        int last = 0;
        int sum = 0;
        for (int i = 0; i < partition.size(); i++) {
            for (int j = last; j < partition.get(i); j++) {
                numOfPartition += sizeOfPartitions[number - sum - j][j];

            }
            sum += partition.get(i);
            last = partition.get(i);
        }
        return numOfPartition;
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
