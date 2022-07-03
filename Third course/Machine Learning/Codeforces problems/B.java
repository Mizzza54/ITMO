import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.StringTokenizer;

/**
 * @author Michael Gerasimov
 */
public class B {
    public static void main(String[] args) {
        FastReader scanner = new FastReader(System.in);
        int k = scanner.nextInt();

        int[][] matrix = new int[k][k];
        int[] truePositive = new int[k],
                falsePositive = new int[k],
                falseNegative = new int[k],
                countClasses = new int[k];
        double[] recall = new double[k],
                precision = new double[k],
                fScore = new double[k];
        int totalSum = 0,
                truePositiveSum = 0,
                falsePositiveSum = 0,
                falseNegativeSum = 0;
        double recallSum = 0,
                precisionSum = 0,
                fScoreSum = 0,
                microRecall,
                microPrecision,
                microAverageFScore,
                macroAverageFScore,
                averageFScore;

        for (int i = 0; i < k; i++) {
            for (int j = 0; j < k; j++) {
                matrix[i][j] = scanner.nextInt();
                totalSum += matrix[i][j];
                countClasses[i] += matrix[i][j];
            }
        }

        for (int i = 0; i < k; i++) {
            for (int j = 0; j < k; j++) {
                if (i == j) {
                    truePositive[i] = matrix[i][j];
                } else {
                    falsePositive[i] += matrix[i][j];
                    falseNegative[i] += matrix[j][i];
                }
            }

            recall[i] = (truePositive[i] + falseNegative[i]) == 0 ? 0 : (double) truePositive[i] / (truePositive[i] + falseNegative[i]);
            precision[i] = (truePositive[i] + falsePositive[i]) == 0 ? 0 : (double) truePositive[i] / (truePositive[i] + falsePositive[i]);
            fScore[i] = (precision[i] + recall[i]) == 0 ? 0 : 2 * ((precision[i] * recall[i]) / (precision[i] + recall[i]));

            truePositiveSum += truePositive[i] * countClasses[i];
            falsePositiveSum += falsePositive[i] * countClasses[i];
            falseNegativeSum += falseNegative[i] * countClasses[i];

            recallSum += recall[i] * countClasses[i];
            precisionSum += precision[i] * countClasses[i];

            fScoreSum += fScore[i] * countClasses[i];
        }

        microRecall = (truePositiveSum + falseNegativeSum) == 0 ? 0 : (double) truePositiveSum / (truePositiveSum + falseNegativeSum);
        microPrecision = (truePositiveSum + falsePositiveSum) == 0 ? 0 : (double) truePositiveSum / (truePositiveSum + falsePositiveSum);
        microAverageFScore = (microPrecision + microRecall) == 0 ? 0 : 2 * ((microPrecision * microRecall) / (microPrecision + microRecall));

        recallSum /= totalSum;
        precisionSum /= totalSum;
        macroAverageFScore = (precisionSum + recallSum) == 0 ? 0 : 2 * ((precisionSum * recallSum) / (precisionSum + recallSum));

        fScoreSum /= totalSum;
        averageFScore = fScoreSum;

        System.out.println(microAverageFScore);
        System.out.println(macroAverageFScore);
        System.out.println(averageFScore);
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