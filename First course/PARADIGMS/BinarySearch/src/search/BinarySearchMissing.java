package search;

/**
 * @author Michale Gerasimov
 * start: 14.02.2020
 * @version -
 */
public class BinarySearchMissing {
    // Pre: arr != null && (for all i < j: a[i] >= a[j])
    // Post: (R = i && a[i - 1] > x >= a[i])
    // (r < a.length  &&  a[r] == x  => R = r) ||
    // || (r >= a.length  ||  a[r] != x => R = -r - 1) &&
    // && for all i = 0...arr.length - 1: arr'[i] = arr[i]
    private static int recBinSearch(int arr[], int l, int r, int x) {
        if (l < r) {
            // Inv: a[-1]..a[l-1] > x >= a[r]..a[a.length] && l < r
            int m = l + (r - l) / 2;
            // r == r' && l == l'
            // l <= m < r
            if (arr[m] <= x) {
                // INV && x >= a[m]
                // a[-1]..a[l-1] > x >= a[m]..a[a.length]
                return recBinSearch(arr, l, m, x);
            } else {
                // INV  &&  x < a[m]
                // a[-1]..a[m] > x >= a[r]..a[a.length]
                return recBinSearch(arr, m + 1, r, x);
            }
        }
        // l' >= r'
        if (r != arr.length && arr[r] == x) {
            return r;
        } else {
            return (-r - 1);
        }
    }

    // Pre: arr != null && (for all i < j: a[i] >= a[j])
    // Post: (R = i && a[i - 1] > x >= a[i])
    // (r < a.length  &&  a[r] == x  => R = r) ||
    // || (r >= a.length  ||  a[r] != x => R = -r - 1) &&
    // && for all i = 0...arr.length - 1: arr'[i] = arr[i]
    private static int iterBinSearch(int arr[], int l, int r, int x) {
        // Inv: a[-1]..a[l-1] > x >= a[r]..a[a.length]
        while (l < r) {
            // INV && l < r
            int m = l + (r - l) / 2;
            // r == r' && l == l'
            // l <= m < r
            if (arr[m] <= x) {
                // INV && x >= a[m]
                r = m;
                // a[-1]..a[l-1] > x >= a[m]..a[a.length]
            } else {
                // INV  &&  x < a[m]
                l = m + 1;
                // a[-1]..a[m] > x >= a[r]..a[a.length]
            }
        }
        // l' >= r'
        // INV && l >= r
        if (r != arr.length && arr[r] == x) {
            return r;
        } else {
            return (-r - 1);
        }
    }

    //Pre: args.length > 0
    public static void main(String[] args) {
        if (args.length <= 1) {
            System.out.println("-1");
            return;
        }
        int x = Integer.parseInt(args[0]);
        int[] a = new int[args.length - 1];
        for (int i = 1; i < args.length; i++) {
            a[i - 1] = Integer.parseInt(args[i]);
        }
        int res = iterBinSearch(a, 0, a.length, x);
        System.out.println(res);
    }
}
