package D;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 08.12.2020
 * @version -
 */
public class D {
    public static int TheNumberOfVertex;
    public static int[][] ListOfEdges;

    public static void main(String[] args) {
        FastReader scanner = new FastReader(System.in);

        TheNumberOfVertex = scanner.nextInt();
        if (TheNumberOfVertex == 1) {
            System.out.println(1);
            System.exit(0);
        }
        ListOfEdges = new int[TheNumberOfVertex][TheNumberOfVertex];

        String string;
        for (int i = 0; i < TheNumberOfVertex; i++) {
            string = scanner.next();
            for (int j = 0; j < string.length(); j++) {
                if (string.charAt(j) == '1') {
                    ListOfEdges[i][j] = 1;
                } else {
                    ListOfEdges[j][i] = 1;
                }
            }
        }

        int[] Answer = findHamiltonianCycle(TheNumberOfVertex);

        for (int value : Answer) {
            System.out.print((value + 1) + " ");
        }
    }

    public static int[] findHamiltonianCycle(int n) {
        int[] Answer = new int[n];
        for (int i = 0; i < n; i++) {
            Answer[i] = i;
        }

        do {
            shuffleArray(Answer);
            mergeSortRecursive(Answer, 0, TheNumberOfVertex);
        } while (ListOfEdges[Answer[n - 1]][Answer[0]] == 0);
        return Answer;
    }

    private static void shuffleArray(int[] array) {
        int index;
        Random random = new Random();
        for (int i = array.length - 1; i > 0; i--)
        {
            index = random.nextInt(i + 1);
            if (index != i)
            {
                array[index] ^= array[i];
                array[i] ^= array[index];
                array[index] ^= array[i];
            }
        }
    }

    public static boolean compare(int left, int right) {
        return ListOfEdges[left][right] == 1;
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

    FastReader(InputStream input) {
        br = new BufferedReader(new InputStreamReader(input));
    }

    String next() {
        try {
            return br.readLine();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return "";
    }

    int nextInt() {
        return Integer.parseInt(next());
    }
}