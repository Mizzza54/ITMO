import java.util.Scanner;

/**
 * @author Michael Gerasimov
 * start: 21.04.2021
 * @version -
 */
public class I {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int r1 = scanner.nextInt();
        int s1 = scanner.nextInt();
        int p1 = scanner.nextInt();

        int r2 = scanner.nextInt();
        int s2 = scanner.nextInt();
        int p2 = scanner.nextInt();


        //System.out.println((r1 + s1 + p1) - (Math.min(s1, r2) + Math.min(r1, p2) + Math.min(p1, s2)));
        System.out.println(Math.max(0, Math.max(r1 - r2 - p2, Math.max(p1 - p2 - s2, s1 - s2 - r2))));
    }
}
