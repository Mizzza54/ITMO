import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Scanner;

/**
 * @author Michale Gerasimov
 * start: 18.12.2019
 * @version -
 */
public class NextPartition {
    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("nextpartition.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("nextpartition.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        String[] line = scanner.nextLine().split("[+=]");
        int n = Integer.parseInt(line[0]);
        ArrayList<Integer> core = new ArrayList<>();
        for (int i = 1; i < line.length; i++) {
            core.add(Integer.parseInt(line[i]));
        }
        if (n != core.get(0)) {
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
            writer.print(n + "=");
            writer.print(core.get(0));
            for (int i = 1; i < core.size(); i++) {
                writer.print("+" + core.get(i));
            }
            writer.println();
        } else {
            writer.print("No solution");
        }
        writer.close();
    }
}
