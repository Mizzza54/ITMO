import java.io.*;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 06.06.2020
 * @version -
 */
public class G {
    static int r, ans;
    static int[] factorial = new int[] {1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800};

    static int[] help = new int[] {0, 4, 9, 12, 18, 20, 25, 28, 32, 36, 44, 45, 49, 50, 52, 60, 63, 68, 72, 75, 76, 84, 90, 92, 96, 98, 99, 100, 108, 116, 117, 121, 124, 126, 132, 140, 144, 147, 148, 150, 153, 156, 160, 164, 169, 171, 172, 175, 180, 188, 196, 198, 200, 204, 207, 212, 220, 224, 225, 228, 234, 236, 242, 243, 244, 245, 252, 256, 260, 261, 268, 275, 276, 279, 284, 288, 289, 292, 294, 300, 306, 308, 315, 316, 324, 325, 332, 333, 338, 340, 342, 348, 350, 352, 356, 360, 361, 363, 364, 369, 372, 380, 387, 388, 392, 396, 400, 404, 412, 414, 416, 420, 423, 425, 428, 436, 441, 444, 450, 452, 460, 468, 475, 476, 477, 480, 484, 486, 490, 492, 495, 500, 504, 507, 508, 516, 522, 524, 525, 529, 531, 532, 539, 540, 544, 548, 549, 550, 556, 558, 564, 572, 575, 576, 578, 580, 585, 588, 596, 600, 603, 604, 605, 608, 612, 620, 628, 630, 636, 637, 639, 644, 650, 652, 657, 660, 666, 668, 672, 675, 676, 684, 692, 693, 700, 708, 711, 716, 720, 722, 724, 725, 726, 732, 735, 736, 738, 740, 747, 748, 756, 764, 765, 768, 772, 774, 775, 780, 784, 788, 792, 796, 800, 801, 804, 812, 819, 820, 825, 828, 833, 836, 841, 844, 845, 846, 847, 850, 852, 855, 860, 864, 867, 868, 873, 876, 882, 884, 892, 900, 908, 909, 916, 924, 925, 927, 928, 931, 932, 936, 940, 948, 950, 954, 956, 961, 963, 964, 968, 972, 975, 980, 981, 988, 990, 992, 996};
    static HashSet<Integer> beautiful = new HashSet<Integer>();


    public static void main(String[] args) throws IOException {
        for (int i = 0; i < help.length; i++) {
            beautiful.add(help[i]);
        }
        help = new int[1];
        FastReader scanner = new FastReader("beautiful.in");
        PrintWriter writer = new PrintWriter("beautiful.out");
        int n = scanner.nextInt();
        r = scanner.nextInt();

        int[] arr = new int[n];
        for (int i = 0; i < n; i++) {
            arr[i] = i + 1;
        }

        if (beautiful.contains(cost(arr))) {
            ans++;
        }

        for (int i = 0; i < factorial[n] - 1; i++) {
            arr = nextPermutation(arr, n);
            if (beautiful.contains(cost(arr))) {
                ans++;
            }
        }

        writer.println(ans);
        scanner.close();
        writer.close();
    }

    static int cost (int[] arr) {
        int n = arr.length;
        int result = 0;
        for (int i = 0; i < n - 1; i++) {
            result += arr[i] * arr[i + 1];
        }
        return result % r;
    }

    static int[] nextPermutation(int[] arr, int n) {
        for (int i = n - 2; i > -1; i--) {
            if (arr[i] < arr[i + 1]) {
                int min = i + 1;
                for (int j = i + 1; j < n; j++) {
                    if ((arr[j] < arr[min]) && (arr[j] > arr[i])){
                        min = j;
                    }
                }
                swap(arr, i, min);// swap a[i] a[min]
                arr = reverse(arr, i + 2, n);
                return arr;
            }
        }
        return null;
    }

    static void swap(int[] arr, int i, int j) {
        int tmp = arr[i];
        arr[i] = arr[j];
        arr[j] = tmp;
    }

    static int[] reverse(int[] arr, int start, int end) {
        int j = start - 1;
        int[] res = new int[arr.length];
        System.arraycopy(arr, 0, res, 0, arr.length);

        for (int i = end - 1; i > start - 2; i--, j++) {
            res[j] = arr[i];
        }

        return res;
    }
    static class FastReader {
        BufferedReader br;
        StringTokenizer st;

        FastReader(String name) throws FileNotFoundException {
            br = new BufferedReader(new
                    InputStreamReader(new
                    FileInputStream(name)));
        }

        FastReader(InputStream input) {
            br = new BufferedReader(new InputStreamReader(input));
        }

        String next() {
            while (st == null || !st.hasMoreElements()) {
                try {
                    st = new StringTokenizer(br.readLine());
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
            return st.nextToken();
        }

        int nextInt() {
            return Integer.parseInt(next());
        }

        void close() throws IOException {
            br.close();
        }
    }
}
