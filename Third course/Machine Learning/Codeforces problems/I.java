import java.io.*;
import java.util.*;


/**
 * @author Michael Gerasimov
 */
public class I {
    private static final FastReader scanner = new FastReader(System.in);
    private static final PrintWriter writer = new PrintWriter(System.out);

    public static void main(String[] args) {
        int n, d, l;
        n = scanner.nextInt();
        d = scanner.nextInt();

        List<AbstractNode> nodes = new ArrayList<>();
        nodes.add(new Image(n, d, scanner));
        nodes.get(0).forwardPass();

        l = scanner.nextInt();
        for (int i = 0; i < l; i++) {
            String type = scanner.next();
            AbstractNode node = null;
            switch (type) {
                case "relu":
                    node = new Relu(nodes, scanner);
                    break;
                case "pool":
                    node = new Pool(nodes, scanner);
                    break;
                case "bias":
                    node = new Bias(nodes, scanner);
                    break;
                case "cnvm":
                case "cnve":
                case "cnvc":
                    node = new Cnv(type, nodes, scanner);
                     break;
            }
            nodes.add(node);
            assert node != null;
            node.forwardPass();
        }
        nodes.get(nodes.size() - 1).readDerivative(scanner);

        for (int i = nodes.size() - 1; i > 0 ; i--) {
            nodes.get(i).backwardsPass();
            nodes.get(i).giveOutputData(nodes, i);
        }

        for (int i = 0; i < nodes.get(nodes.size() - 1).outputDepth; i++) {
            for (int j = 0; j < nodes.get(nodes.size() - 1).outputHeight; j++) {
                for (int k = 0; k < nodes.get(nodes.size() - 1).outputWidth; k++) {
                    writer.write(nodes.get(nodes.size() - 1).outputData[i][j][k] + " ");
                }
            }
        }
        writer.println();

        for (AbstractNode node : nodes) {
            node.printDerivative(writer);
        }

        writer.flush();
        writer.close();
    }
}

abstract class AbstractNode {
    protected double[][][] inputData, outputData, derivativeOutputData, derivativeInputData;
    protected int inputHeight, inputWidth, inputDepth, outputHeight, outputWidth, outputDepth;

    public AbstractNode() {}

    public AbstractNode(List<AbstractNode> nodes) {
        AbstractNode last = nodes.get(nodes.size() - 1);
        inputData = Utils.copy3d(last.outputData);
        inputHeight = inputData[0].length;
        inputWidth = inputData[0][0].length;
        inputDepth = inputData.length;
        derivativeOutputData = new double[inputDepth][inputHeight][inputWidth];
    }

    abstract protected void forwardPass();

    abstract protected void backwardsPass();

    abstract protected void printDerivative(PrintWriter writer);

    protected void giveOutputData(List<AbstractNode> nodes, int id) {
        nodes.get(id - 1).derivativeInputData = Utils.copy3d(derivativeOutputData);
    }

    protected void readDerivative(FastReader scanner) {
        derivativeInputData = new double[outputDepth][outputHeight][outputWidth];
        for (int i = 0; i < outputDepth; i++) {
            for (int j = 0; j < outputHeight; j++) {
                for (int k = 0; k < outputWidth; k++) {
                    derivativeInputData[i][j][k] = scanner.nextInt();
                }
            }
        }
    }
}

class Image extends AbstractNode {

    public Image(int n, int d, FastReader scanner) {
        inputHeight = n;
        inputWidth = n;
        inputDepth = d;
        outputHeight = n;
        outputWidth = n;
        outputDepth = d;
        inputData = new double[d][n][n];
        for (int i = 0; i < d; i++) {
            for (int j = 0; j < n; j++) {
                for (int k = 0; k < n; k++) {
                    inputData[i][j][k] = scanner.nextInt();
                }
            }
        }
    }

    @Override
    protected void forwardPass() {
        outputData = Utils.copy3d(inputData);
    }

    @Override
    protected void backwardsPass() {}

    @Override
    protected void printDerivative(PrintWriter writer) {
        for (int i = 0; i < inputDepth; i++) {
            for (int j = 0; j < inputHeight; j++) {
                for (int k = 0; k < inputWidth; k++) {
                    writer.print(derivativeInputData[i][j][k] + " ");
                }
            }
        }
        writer.println();
    }
}

class Relu extends AbstractNode {
    int alpha;

    public Relu(List<AbstractNode> nodes, FastReader scanner) {
        super(nodes);
        this.alpha = scanner.nextInt();
        outputHeight = inputHeight;
        outputWidth = inputWidth;
        outputDepth = inputDepth;
        outputData = new double[outputDepth][outputHeight][outputWidth];
    }

    @Override
    protected void forwardPass() {
        for (int i = 0; i < outputDepth; i++) {
            for (int j = 0; j < outputHeight; j++) {
                for (int k = 0; k < outputWidth; k++) {
                    outputData[i][j][k] = inputData[i][j][k] >= 0 ? inputData[i][j][k] : (1.0 / alpha) * inputData[i][j][k];
                }
            }
        }
    }

    @Override
    protected void backwardsPass() {
        derivativeOutputData = new double[outputDepth][outputHeight][outputWidth];
        for (int i = 0; i < outputDepth; i++) {
            for (int j = 0; j < outputHeight; j++) {
                for (int k = 0; k < outputWidth; k++) {
                    derivativeOutputData[i][j][k] = derivativeInputData[i][j][k] * (inputData[i][j][k] >= 0 ? 1 : (1.0 / alpha));
                }
            }
        }
    }

    @Override
    protected void printDerivative(PrintWriter writer) {}
}

class Pool extends AbstractNode {
    int s;

    public Pool(List<AbstractNode> nodes, FastReader scanner) {
        super(nodes);
        this.s = scanner.nextInt();
        outputHeight = (inputHeight - s) / s + 1;
        outputWidth = (inputWidth - s) / s + 1;
        outputDepth = inputDepth;
        outputData = new double[outputDepth][outputHeight][outputWidth];
    }

    @Override
    protected void forwardPass() {
        for (int d = 0; d < outputDepth; d++) {
            for (int i = 0; i < outputHeight; i++) {
                for (int j = 0; j < outputWidth; j++) {
                    double max = Double.NEGATIVE_INFINITY;
                    for (int i2 = i * s; i2 < i * s + s; i2++) {
                        for (int j2 = j * s; j2 < j * s + s; j2++) {
                            max = Math.max(inputData[d][i2][j2], max);
                        }
                    }
                    outputData[d][i][j] = max;
                }
            }
        }
    }

    @Override
    protected void backwardsPass() {
        for (int d = 0; d < outputDepth; d++) {
            for (int i = 0; i < outputHeight; i++) {
                for (int j = 0; j < outputWidth; j++) {
                    for (int i2 = i * s; i2 < i * s + s; i2++) {
                        for (int j2 = j * s; j2 < j * s + s; j2++) {
                            if (inputData[d][i2][j2] == outputData[d][i][j]) {
                                derivativeOutputData[d][i2][j2] = derivativeInputData[d][i][j];
                            }
                        }
                    }
                }
            }
        }
    }

    @Override
    protected void printDerivative(PrintWriter writer) {}
}

class Bias extends AbstractNode {
    int[] b;
    double[] derivativeB;

    public Bias(List<AbstractNode> nodes, FastReader scanner) {
        super(nodes);
        b = new int[inputData.length];
        derivativeB = new double[inputData.length];
        for (int i = 0; i < b.length; i++) {
            b[i] = scanner.nextInt();
        }
        outputHeight = inputHeight;
        outputWidth = inputWidth;
        outputDepth = inputDepth;
        outputData = new double[outputDepth][outputHeight][outputWidth];
    }

    @Override
    protected void forwardPass() {
        for (int i = 0; i < outputDepth; i++) {
            for (int j = 0; j < outputHeight; j++) {
                for (int k = 0; k < outputWidth; k++) {
                    outputData[i][j][k] = inputData[i][j][k] + b[i];
                }
            }
        }
    }

    @Override
    protected void backwardsPass() {
        for (int i = 0; i < outputDepth; i++) {
            for (int j = 0; j < outputHeight; j++) {
                for (int k = 0; k < outputWidth; k++) {
                    derivativeB[i] += derivativeInputData[i][j][k];
                    derivativeOutputData[i][j][k] = derivativeInputData[i][j][k];
                }
            }
        }
    }

    @Override
    protected void printDerivative(PrintWriter writer) {
        for (double j : derivativeB) {
            writer.write(j + " ");
        }
        writer.println();
    }
}

class Cnv extends AbstractNode {
    int hCore, kCore, step, padding;
    double[][][][] a;
    double[][][][] derivativeA;
    String type;

    double[][][] paddingInputData;

    public Cnv(String type, List<AbstractNode> nodes, FastReader scanner) {
        super(nodes);
        this.type = type;

        hCore = scanner.nextInt();
        kCore = scanner.nextInt();
        step = scanner.nextInt();
        padding = scanner.nextInt();

        a = new double[hCore][inputDepth][kCore][kCore];
        derivativeA = new double[hCore][inputDepth][kCore][kCore];

        for (int i = 0; i < hCore; i++) {
            for (int j = 0; j < inputDepth; j++) {
                for (int l = 0; l < kCore; l++) {
                    for (int m = 0; m < kCore; m++) {
                        a[i][j][l][m] = scanner.nextInt();
                    }
                }
            }
        }

        outputHeight = (inputHeight - kCore + 2 * padding) / step + 1;
        outputWidth = (inputWidth - kCore + 2 * padding) / step + 1;
        outputDepth = hCore;
        outputData = new double[outputDepth][outputHeight][outputWidth];
    }

    @Override
    protected void forwardPass() {
        double[][][] paddingData = getPadding();
        paddingInputData = Utils.copy3d(paddingData);
        for (int od = 0; od < outputDepth; od++) {
            for (int id = 0; id < inputDepth; id++) {
                for (int i = 0; i < outputHeight; i++) {
                    for (int j = 0; j < outputWidth; j++) {
                        for (int i2 = 0, i3 = i * step; i2 < kCore; i2++, i3++) {
                            for (int j2 = 0, j3 = j * step; j2 < kCore; j2++, j3++) {
                                outputData[od][i][j] += paddingData[id][i3][j3] * a[od][id][i2][j2];
                            }
                        }
                    }
                }
            }
        }
    }

    @Override
    protected void backwardsPass() {
        double[][][] paddingData = makeCompress(getBackwardsPadding());
        for (int i = 0; i < inputDepth; i++) {
            for (int j = 0; j < inputHeight; j++) {
                for (int k = 0; k < inputWidth; k++) {
                    derivativeOutputData[i][j][k] += paddingData[i][j + padding][k + padding];
                }
            }
        }
    }

    @Override
    protected void printDerivative(PrintWriter writer) {
        for (int i = 0; i < hCore; i++) {
            for (int j = 0; j < inputDepth; j++) {
                for (int k = 0; k < kCore; k++) {
                    for (int l = 0; l < kCore; l++) {
                        writer.write(derivativeA[i][j][k][l] + " ");
                    }
                }
            }
        }
        writer.println();
    }

    private double[][][] getBackwardsPadding() {
        double[][][] result = new double[inputDepth][inputHeight + 2 * padding][inputWidth + 2 * padding];
        for (int od = 0; od < outputDepth; od++) {
            for (int id = 0; id < inputDepth; id++) {
                for (int i = 0; i < outputHeight; i++) {
                    for (int j = 0; j < outputWidth; j++) {
                        for (int i2 = 0, i3 = i * step; i2 < kCore; i2++, i3++) {
                            for (int j2 = 0, j3 = j * step; j2 < kCore; j2++, j3++) {
                                result[id][i3][j3] += a[od][id][i2][j2] * derivativeInputData[od][i][j];
                                derivativeA[od][id][i2][j2] += paddingInputData[id][i3][j3] * derivativeInputData[od][i][j];
                            }
                        }
                    }
                }
            }
        }
        return result;
    }

    private double[][][] makeCompress(double[][][] matrix) {
        switch (type) {
            case "cnvm":
                return makeMirrorCompress(matrix);
            case "cnve":
                return makeBorderExtensionCompress(matrix);
            case "cnvc":
                return makeCyclicShiftCompress(matrix);
        }
        throw new IllegalStateException();
    }

    private double[][][] makeMirrorCompress(double[][][] matrix) {
        for (int d = 0; d < inputDepth; d++) {
            int n = inputHeight;
            int m = inputWidth;

            for (int j = 0; j < m + 2 * padding; j++) {
                for (int i = 0; i < padding; i++) {
                        matrix[d][2 * padding - i][j] += matrix[d][i][j];
                }

                for (int i = n + padding; i < n + 2 * padding; i++) {
                    matrix[d][2 * (n + padding) - i - 2][j] += matrix[d][i][j];
                }
            }

            for (int i = padding; i < padding + n; i++) {
                for (int j = 0; j < padding; j++) {
                    matrix[d][i][padding - j + padding] += matrix[d][i][j];
                }

                for (int j = m + padding; j < m + 2 * padding; j++) {
                    matrix[d][i][2 * m + padding - j - 2 + padding] += matrix[d][i][j];
                }
            }
        }
        return matrix;
    }

    private double[][][] makeBorderExtensionCompress(double[][][] matrix) {
        for (int d = 0; d < inputDepth; d++) {
            int n = inputHeight;
            int m = inputWidth;

            for (int j = 0; j < m + 2 * padding; j++) {
                for (int i = 0; i < padding; i++) {
                    matrix[d][padding][j] += matrix[d][i][j];
                }

                for (int i = n + padding; i < n + 2 * padding; i++) {
                    matrix[d][n + padding - 1][j] += matrix[d][i][j];
                }
            }

            for (int i = padding; i < padding + n; i++) {
                for (int j = 0; j < padding; j++) {
                    matrix[d][i][padding] += matrix[d][i][j];
                }

                for (int j = m + padding; j < m + 2 * padding; j++) {
                    matrix[d][i][m + padding - 1] += matrix[d][i][j];
                }
            }
        }
        return matrix;
    }

    private double[][][] makeCyclicShiftCompress(double[][][] matrix) {
        for (int d = 0; d < inputDepth; d++) {
            int n = inputHeight;
            int m = inputWidth;

            for (int j = 0; j < m + 2 * padding; j++) {
                for (int i = padding - 1; i > -1; i--) {
                    matrix[d][i + n][j] += matrix[d][i][j];
                }

                for (int i = n + padding; i < n + 2 * padding; i++) {
                    matrix[d][i - n][j] += matrix[d][i][j];
                }
            }

            for (int i = padding; i < padding + n; i++) {
                for (int j = padding - 1; j > -1; j--) {
                    matrix[d][i][j + m] += matrix[d][i][j];
                }

                for (int j = m + padding; j < m + 2 * padding; j++) {
                    matrix[d][i][j - m] += matrix[d][i][j];
                }
            }
        }
        return matrix;
    }

    private double[][][] getPadding() {
        switch (type) {
            case "cnvm":
                return makeMirrorShift();
            case "cnve":
                return makeBorderExtensionShift();
            case "cnvc":
                return makeCyclicShift();
        }
        throw new IllegalStateException();
    }

    private double[][][] initPaddingData() {
        double[][][] result = new double[inputDepth][inputHeight + 2 * padding][inputWidth + 2 * padding];
        for (int i = 0; i < inputDepth; i++) {
            for (int j = 0; j < inputHeight; j++) {
                for (int k = 0; k < inputWidth; k++) {
                    result[i][j + padding][k + padding] = inputData[i][j][k];
                }
            }
        }
        return result;
    }

    private double[][][] makeMirrorShift() {
        double[][][] paddingData = initPaddingData();
        int paddingDataHeight = paddingData[0].length;
        int paddingDataWidth = paddingData[0][0].length;
        int paddingDataDepth = paddingData.length;

        for (int k = 0; k < paddingDataDepth; k++) {
            for (int i = padding; i < inputHeight + padding; i++) {
                // Левая граница
                for (int j = 0; j < padding; j++) {
                    paddingData[k][i][j] += inputData[k][i - padding][padding - j];
                }

                // Правая граница
                for (int j = padding + inputWidth; j < paddingDataWidth; j++) {
                    paddingData[k][i][j] += inputData[k][i - padding][2 * inputWidth + padding - j - 2];
                }
            }


            for (int j = 0; j < paddingDataWidth; j++) {
                // Верхняя граница
                for (int i = 0; i < padding; i++) {
                    paddingData[k][i][j] += paddingData[k][2 * padding - i][j];
                }

                // Нижняя граница
                for (int i = inputHeight + padding; i < paddingDataHeight; i++) {
                    paddingData[k][i][j] += paddingData[k][2 * (inputHeight + padding) - i - 2][j];
                }
            }
        }

        return paddingData;
    }

    private double[][][] makeBorderExtensionShift() {
        double[][][] paddingData = initPaddingData();
        int paddingDataHeight = paddingData[0].length;
        int paddingDataWidth = paddingData[0][0].length;
        int paddingDataDepth = paddingData.length;

        for (int k = 0; k < paddingDataDepth; k++) {
            for (int i = padding; i < inputHeight + padding; i++) {
                // Левая граница
                for (int j = 0; j < padding; j++) {
                    paddingData[k][i][j] += inputData[k][i - padding][0];
                }

                // Правая граница
                for (int j = padding + inputWidth; j < paddingDataWidth; j++) {
                    paddingData[k][i][j] += inputData[k][i - padding][inputWidth - 1];
                }
            }


            for (int j = 0; j < paddingDataWidth; j++) {
                // Верхняя граница
                for (int i = 0; i < padding; i++) {
                    paddingData[k][i][j] += paddingData[k][padding][j];
                }

                // Нижняя граница
                for (int i = inputHeight + padding; i < paddingDataHeight; i++) {
                    paddingData[k][i][j] += paddingData[k][inputHeight + padding - 1][j];
                }
            }
        }

        return paddingData;
    }

    private double[][][] makeCyclicShift() {
        double[][][] paddingData = initPaddingData();
        int paddingDataHeight = paddingData[0].length;
        int paddingDataWidth = paddingData[0][0].length;
        int paddingDataDepth = paddingData.length;

        for (int k = 0; k < paddingDataDepth; k++) {
            for (int i = padding; i < inputHeight + padding; i++) {
                // Левая граница
                for (int j = padding - 1; j > -1; j--) {
                    paddingData[k][i][j] += paddingData[k][i][inputWidth + j];
                }

                // Правая граница
                for (int j = padding + inputWidth; j < paddingDataWidth; j++) {
                    paddingData[k][i][j] += paddingData[k][i][j - inputWidth];
                }
            }


            for (int j = 0; j < paddingDataWidth; j++) {
                // Верхняя граница
                for (int i = padding - 1; i > -1; i--) {
                    paddingData[k][i][j] += paddingData[k][i + inputHeight][j];
                }

                // Нижняя граница
                for (int i = inputHeight + padding; i < paddingDataHeight; i++) {
                    paddingData[k][i][j] += paddingData[k][i - inputHeight][j];
                }
            }
        }

        return paddingData;
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

class Utils {
    public static double[] copy1d(double[] a) {
        double[] res = new double[a.length];
        System.arraycopy(a, 0, res, 0, a.length);
        return res;
    }

    public static double[][] copy2d(double[][] a) {
        double[][] res = new double[a.length][a[0].length];
        for (int i = 0; i < res.length; i++) {
            res[i] = copy1d(a[i]);
        }
        return res;
    }

    public static double[][][] copy3d(double[][][] a) {
        double[][][] res = new double[a.length][a[0].length][a[0][0].length];
        for (int i = 0; i < res.length; i++) {
            res[i] = copy2d(a[i]);
        }
        return res;
    }
}