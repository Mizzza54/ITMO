import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Scanner;

/**
 * @author Michale Gerasimov
 * start: 08.12.2019
 * @version -
 */
public class Partition {
    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("partition.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("partition.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        int n = scanner.nextInt();
        ArrayList<Integer> core = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            core.add(1);
        }
        
        writer.print("1");
        for (int i = 1; i < n; i++) {
            writer.print("+1");
        }
        writer.println();
        
        while (core.get(0) != n) {
            core.set(core.size() - 1, core.get(core.size() - 1) - 1);
            core.set(core.size() - 2, core.get(core.size() - 2) + 1);
            if (core.get(core.size() - 2) - core.get(core.size() - 1) > 0) {
                core.set(core.size() - 2, core.get(core.size() - 1) + core.get(core.size() - 2));
                core.remove(core.size() - 1);
            } else {
                while (core.get(core.size() - 2) * 2 <= core.get(core.size() - 1)) {
                    core.add(core.get(core.size() - 1) - core.get(core.size() - 2));
                    core.set(core.size() - 2, core.get(core.size() - 3));
                }
            }
            writer.print(core.get(0));
            for (int i = 1; i < core.size(); i++) {
                writer.print("+" + core.get(i));
            }
            writer.println();
        }
        writer.close();
    }
}