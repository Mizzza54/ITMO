import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 12.12.2019
 * @version -
 */
public class NextSetPartition {
    public static Scanner scanner;

    static {
        try {
            scanner = new Scanner(new FileInputStream("nextsetpartition.in"));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static PrintWriter writer;

    static {
        try {
            writer = new PrintWriter("nextsetpartition.out");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        int n;
        n = scanner.nextInt();
        ArrayList<Integer> one = new ArrayList<>();
        ArrayList<ArrayList<Integer>> ans = new ArrayList<>();
        ans.add(one);
        System.out.println(ans);
        ans = nextSubset(ans);
        System.out.println(ans);
    }

    public static ArrayList<ArrayList<Integer>> nextSubset(ArrayList<ArrayList<Integer>> a) {
        ArrayList<Integer> used = new ArrayList<>();
        boolean fl = false;
        for (int i = a.size() - 1; i > -1; i--) {
            if (used.size() != 0 && used.get(used.size() - 1) > a.get(i).get(a.get(i).size() - 1)) {
                a.get(i).add(used.get(used.size() - 1));
                used.remove(used.size() - 1);
                break;
            }
            int j;
            for (j = a.get(i).size() - 1; j > -1; j--) {
                if (used.size() != 0 && j != 0 && used.get(used.size() - 1) > a.get(i).get(j)) {
                    a.get(i).add(j, used.get(used.size() - 1));
                    fl = true;
                    break;
                }
            }
            if (fl = true) {
                break;
            }
            used.add(a.get(i).get(j));
            a.get(i).remove(j);
        }
        Collections.sort(used);
        for (int i = 0; i < used.size(); i++) {
            ArrayList<Integer> tmp = new ArrayList<>();
            tmp.add(used.get(i));
            a.add(tmp);
        }
        return a;
    }
}
