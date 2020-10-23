package I;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.StringTokenizer;

/**
 * @author Michael Gerasimov
 * start: 17.10.2020
 * @version -
 */
public class I {
    static FastReader scanner = new FastReader(System.in);
    static double answer = 0;

    public static void main(String[] args) {
        PrimFindMst();

        System.out.println(answer);
    }

    public static void PrimFindMst() {
        int NumberOfVertex = scanner.nextInt();
        int[] xtemp = new int[NumberOfVertex];
        int[] ytemp = new int[NumberOfVertex];
        double[] minWeight = new double[NumberOfVertex];
        boolean[] used = new boolean[NumberOfVertex];
        for (int i = 0; i < NumberOfVertex; i++) {
            xtemp[i] = scanner.nextInt();
            ytemp[i] = scanner.nextInt();
            minWeight[i] = Distance(xtemp[0], ytemp[0], xtemp[i], ytemp[i]);
            used[i] = false;
        }
        used[0] = true;


        for (int i = 1; i < NumberOfVertex; i++) {
            int curVertex = 0;
            double curWeight = Double.MAX_VALUE;
            for (int j = 1; j < NumberOfVertex; j++) {
                if (!used[j] && (curWeight > minWeight[j])) {
                    curVertex = j;
                    curWeight = minWeight[j];
                }
            }

            used[curVertex] = true;
            answer += curWeight;

            for (int j = 1; j < NumberOfVertex; j++) {
                double dist =  Distance(xtemp[curVertex], ytemp[curVertex], xtemp[j], ytemp[j]);
                if (dist < minWeight[j]) {
                    minWeight[j] = dist;
                }
            }
        }
    }

    public static double Distance(int x1, int y1, int x2, int y2) {
        return Math.sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2));
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