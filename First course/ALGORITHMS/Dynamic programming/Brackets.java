import java.util.ArrayList;
import java.util.Scanner;

/**
 * @author Michale Gerasimov
 * start: 19.12.2019
 * @version -
 */

public class Brackets {
    private static int n;
    private static String brackets;
    private static int[][] dp;
    private static int[][] prev;
    public static ArrayList<Character> ans = new ArrayList<>();

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        brackets = sc.nextLine();
        n = brackets.length();
        if (!brackets.equals("")) {
            dp = new int[n][n];
            prev = new int[n][n];
            for (int r = 0; r < n; r++) {
                for (int l = r; l > -1; l--) {
                    if (l == r) {
                        dp[l][r] = 1;
                    } else {
                        int best = 10000000 ;
                        int curprev = -1;
                        if (brackets.charAt(l) == '(' && brackets.charAt(r) == ')'
                                || brackets.charAt(l) == '[' && brackets.charAt(r) == ']'
                                || brackets.charAt(l) == '{' && brackets.charAt(r) == '}') {
                            best = dp[l + 1][r - 1];
                        }
                        for (int k = l; k < r; k++) {
                            if (dp[l][k] + dp[k + 1][r] < best) {
                                best = dp[l][k] + dp[k + 1][r];
                                curprev = k;
                            }
                        }
                        dp[l][r] = best;
                        prev[l][r] = curprev;
                    }
                }
            }
            findprev(0, n - 1);
            for (int i = 0; i < ans.size(); i++) {
                System.out.print(ans.get(i));
            }
        }
    }

    private static void findprev(int l, int r) {
        if (dp[l][r] == r - l + 1) {
            return;
        }
        if (dp[l][r] == 0) {
            for (int i = l; i <= r; i++) {
                ans.add(brackets.charAt(i));
            }
            return;
        }
        if (prev[l][r] == -1) {
            ans.add(brackets.charAt(l));
            findprev(l + 1, r - 1);
            ans.add(brackets.charAt(r));
            return;
        }
        findprev(l, prev[l][r]);
        findprev(prev[l][r] + 1, r);
    }
}