package C;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 08.12.2020
 * @version -
 */
public class C {
    public static FastReader scanner = new FastReader(System.in);

    public static void main(String[] args) {
        int n = scanner.nextInt();
        int[] array = new int[n];
        for (int i = 0; i < n; i++) {
            array[i] = i + 1;
        }
        mergeSortRecursive(array, 0, array.length);

        System.out.print("0 ");
        for (int i = 0; i < n; i++) {
            System.out.print(array[i] + " ");
        }
    }

    public static boolean compare(int left, int right) {
        System.out.println("1 " + left + " " + right);
        System.out.flush();
        String input = scanner.next();
        return input.equals("YES");
    }

    public static void mergeSortRecursive(int[] array, int left, int right) {
        if (left + 1 >= right) {
            return;
        }

        int mid = (left + right) / 2;
        mergeSortRecursive(array, left, mid);
        mergeSortRecursive(array, mid, right);
        merge(array, left, mid, right);
    }

    public static void merge(int[] array, int left, int mid, int right) {
        int it1 = 0;
        int it2 = 0;
        int[] result = new int[right - left];

        while (left + it1 < mid && mid + it2 < right) {
            if (compare(array[left + it1], array[mid + it2])) {
                result[it1 + it2] = array[left + it1];
                it1 += 1;
            } else {
                result[it1 + it2] = array[mid + it2];
                it2 += 1;
            }
        }

        while (left + it1 < mid) {
            result[it1 + it2] = array[left + it1];
            it1 += 1;
        }

        while (mid + it2 < right) {
            result[it1 + it2] = array[mid + it2];
            it2 += 1;
        }

        for (int i = 0; i < it1 + it2; i++) {
            array[left + i] = result[i];
        }
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