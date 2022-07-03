import java.io.*;
import java.util.*;

/**
 * @author Michael Gerasimov
 */
public class J {

    private static double[][] readVector(int size, FastReader scanner) {
        double[][] res = new double[size][1];
        for (int i = 0; i < size; i++) {
            res[i][0] = scanner.nextInt();
        }
        return res;
    }

    private static double[][] readMatrix(int size, FastReader scanner) {
        double[][] res = new double[size][size];
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                res[i][j] = scanner.nextInt();
            }
        }
        return res;
    }

    private static Node readAndAddMatrix(int size, List<Node> network, FastReader scanner) {
        double[][] matrix = readMatrix(size, scanner);
        Var node = new Var(matrix);
        network.add(node);
        return node;
    }

    private static Node readAndAddVector(int size, List<Node> network, FastReader scanner) {
        double[][] matrix = readVector(size, scanner);
        Var node = new Var(matrix);
        network.add(node);
        return node;
    }

    private static void printMatrix(double[][] matrix, PrintWriter writer) {
        for (double[] doubles : matrix) {
            for (int j = 0; j < matrix[0].length; j++) {
                writer.print(doubles[j] + " ");
            }
            writer.println();
        }
    }

    public static void main(String[] args) {
        FastReader scanner = new FastReader(System.in);
        PrintWriter writer = new PrintWriter(System.out);
        List<Node> network = new ArrayList<>();

        int n = scanner.nextInt();
        for (int i = 0; i < 4; i++) {
            readAndAddMatrix(n, network, scanner);
            readAndAddMatrix(n, network, scanner);
            readAndAddVector(n, network, scanner);
        }

        int m = scanner.nextInt();
        readAndAddVector(n, network, scanner);
        readAndAddVector(n, network, scanner);

        Node temp = network.get(13);
        network.set(13, network.get(12));
        network.set(12, temp);

        List<Integer> fPositions = new ArrayList<>();
        List<Integer> cPositions = new ArrayList<>(Collections.singleton(12));
        List<Integer> hPositions = new ArrayList<>(Collections.singleton(13));
        List<Integer> xPositions = new ArrayList<>();

        for (int i = 0; i < m; i++) {
            Var node = (Var) readAndAddVector(n, network, scanner);

            int startPos = 14 + 21 * i;
            int hPrevPos = startPos - 1;
            int cPrevPos = hPrevPos - 1;

            for (int j = 0; j < 4; j++) {
                network.add(new Mul(network.get(j * 3), node));
                network.add(new Mul(network.get(j * 3 + 1), network.get(hPrevPos)));
                network.add(new Sum(new ArrayList<>(List.of(network.get(network.size() - 2), network.get(network.size() - 1), network.get(j * 3 + 2)))));

                if (j == 3) {
                    network.add(new Tnh(network.get(network.size() - 1)));
                } else {
                    network.add(new Sigm(network.get(network.size() - 1)));
                }
            }

            int fPos = network.size() - 1 - (4 * 3);

            int iPos = network.size() - 1 - (4 * 2);

            int oPos = network.size() - 1 - (4);

            int tanhPos = network.size() - 1;

            int iHadTanhPos = network.size();
            network.add(new Had(new ArrayList<>(List.of(network.get(iPos), network.get(tanhPos)))));

            int fHadCPos = network.size();
            network.add(new Had(new ArrayList<>(List.of(network.get(fPos), network.get(cPrevPos)))));

            int cPos = network.size();
            network.add(new Sum(new ArrayList<>(List.of(network.get(fHadCPos), network.get(iHadTanhPos)))));

            int hPos = network.size();
            network.add(new Had(new ArrayList<>(List.of(network.get(oPos), network.get(cPos)))));

            fPositions.add(oPos);
            cPositions.add(cPos);
            hPositions.add(hPos);
            xPositions.add(startPos);
        }

        for (Node node : network) {
            node.forwardPass();
        }

        network.get(hPositions.get(m)).readDerivative(scanner);
        network.get(cPositions.get(m)).readDerivative(scanner);

        for (int i = m - 1; i > -1; i--) {
            network.get(fPositions.get(i)).readDerivative(scanner);
        }

        for (int i = network.size() - 1; i > -1; i--) {
            network.get(i).backwardsPass();
        }

        for (int i = 0; i < m; i++) {
            printMatrix(network.get(fPositions.get(i)).outputData, writer);
        }
        printMatrix(network.get(hPositions.get(m)).outputData, writer);
        printMatrix(network.get(cPositions.get(m)).outputData, writer);

        for (int i = m - 1; i > -1; i--) {
            printMatrix(network.get(xPositions.get(i)).derivative, writer);
        }
        printMatrix(network.get(hPositions.get(0)).derivative, writer);
        printMatrix(network.get(cPositions.get(0)).derivative, writer);

        for (int i = 0; i < 12; i++) {
            printMatrix(network.get(i).derivative, writer);
        }

        writer.flush();
        writer.close();
    }
}

abstract class Node {
    protected final ArrayList<Node> nodes;
    protected double[][] outputData, derivative;

    public Node(ArrayList<Node> nodes) {
        this.nodes = new ArrayList<>(nodes);
    }

    public abstract void forwardPass();

    public abstract void backwardsPass();

    public void readDerivative(FastReader scanner) {
        for (double[] line : derivative) {
            for (int j = 0; j < line.length; j++) {
                line[j] = scanner.nextInt();
            }
        }
    }
}
class Var extends Node {

    public Var(double[][] d) {
        super(new ArrayList<>());
        outputData = Utils.copy2d(d);
        derivative = new double[outputData.length][outputData[0].length];
    }

    @Override
    public void forwardPass() {}

    @Override
    public void backwardsPass() {}
}

class Tnh extends Node {

    public Tnh(Node node) {
        super(new ArrayList<>(Collections.singleton(node)));
    }

    @Override
    public void forwardPass() {
        outputData = Utils.copy2d(nodes.get(0).outputData);
        derivative = new double[outputData.length][outputData[0].length];
        for (double[] doubles : outputData) {
            for (int i = 0; i < doubles.length; i++) {
                doubles[i] = Math.tanh(doubles[i]);
            }
        }
    }

    @Override
    public void backwardsPass() {
        for (int i = 0; i < outputData.length; i++) {
            for (int j = 0; j < outputData[i].length; j++) {
                nodes.get(0).derivative[i][j] += derivative[i][j] * (1 - Math.pow(outputData[i][j], 2));
            }
        }
    }
}

class Sigm extends Node {

    public Sigm(Node node) {
        super(new ArrayList<>(Collections.singleton(node)));
    }

    @Override
    public void forwardPass() {
        outputData = Utils.copy2d(nodes.get(0).outputData);
        derivative = new double[outputData.length][outputData[0].length];
        for (double[] doubles : outputData) {
            for (int i = 0; i < doubles.length; i++) {
                doubles[i] = 1 / (1 + Math.exp(-doubles[i]));
            }
        }
    }

    @Override
    public void backwardsPass() {
        for (int i = 0; i < outputData.length; i++) {
            for (int j = 0; j < outputData[0].length; j++) {
                nodes.get(0).derivative[i][j] += outputData[i][j] * (1 - outputData[i][j]) * derivative[i][j];
            }
        }
    }
}

class Mul extends Node {

    public Mul(Node left, Node right) {
        super(new ArrayList<>(List.of(left, right)));
    }

    @Override
    public void forwardPass() {
        outputData = multiplyMatrix(nodes.get(0).outputData, nodes.get(1).outputData);
        derivative = new double[outputData.length][outputData[0].length];
    }

    @Override
    public void backwardsPass() {
        double[][] left = multiplyMatrix(derivative, transposeMatrix(nodes.get(1).outputData));
        double[][] right = multiplyMatrix(transposeMatrix(nodes.get(0).outputData), derivative);

        for (int i = 0; i < left.length; i++) {
            for (int j = 0; j < left[0].length; j++) {
                nodes.get(0).derivative[i][j] += left[i][j];
            }
        }

        for (int i = 0; i < right.length; i++) {
            for (int j = 0; j < right[0].length; j++) {
                nodes.get(1).derivative[i][j] += right[i][j];
            }
        }
    }

    private double[][] multiplyMatrix(double[][] a, double[][] b) {
        double[][] res = new double[a.length][b[0].length];
        for (int i = 0; i < a.length; i++) {
            for (int j = 0; j < b[0].length; j++) {
                for (int k = 0; k < b.length; k++) {
                    res[i][j] += a[i][k] * b[k][j];
                }
            }
        }
        return res;
    }

    private double[][] transposeMatrix(double[][] a) {
        double[][] res = new double[a[0].length][a.length];
        for (int i = 0; i < a.length; i++) {
            for (int j = 0; j < a[i].length; j++) {
                res[j][i] = a[i][j];
            }
        }
        return res;
    }
}

class Sum extends Node {

    public Sum(ArrayList<Node> nodes) {
        super(nodes);
    }

    @Override
    public void forwardPass() {
        int n = nodes.get(0).outputData.length;
        int m = nodes.get(0).outputData[0].length;
        outputData = new double[n][m];
        derivative = new double[outputData.length][outputData[0].length];
        for (Node node : nodes) {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    outputData[i][j] += node.outputData[i][j];
                }
            }
        }
    }

    @Override
    public void backwardsPass() {
        for (int i = 0; i < outputData.length; ++i) {
            for (int j = 0; j < outputData[0].length; ++j) {
                for (Node node : nodes) {
                    node.derivative[i][j] += derivative[i][j];
                }
            }
        }
    }
}

class Had extends Node {

    public Had(ArrayList<Node> nodes) {
        super(nodes);
    }

    @Override
    public void forwardPass() {
        List<double[][]> args = new ArrayList<>();
        for (Node node : nodes) {
            args.add(node.outputData);
        }
        outputData = hadamardProduct(args, -1);
        derivative = new double[outputData.length][outputData[0].length];
    }

    @Override
    public void backwardsPass() {
        List<double[][]> args = new ArrayList<>();
        for (Node node : nodes) {
            args.add(node.outputData);
        }

        for (int k = 0; k < args.size(); k++) {
            double[][] temp = hadamardProduct(args, k);

            for (int i = 0; i < temp.length; i++) {
                for (int j = 0; j < temp[i].length; j++) {
                    temp[i][j] *= derivative[i][j];
                    nodes.get(k).derivative[i][j] += temp[i][j];
                }
            }
        }
    }

    private double[][] hadamardProduct(List<double[][]> args, int ignoreIndex) {
        double[][] res = new double[args.get(0).length][args.get(0)[0].length];

        if (args.size() == 2 && (ignoreIndex == 0 || ignoreIndex == 1)) {
            res = Utils.copy2d(args.get(ignoreIndex == 0 ? 1 : 0));
            return res;
        }

        for (double[] row : res) {
            Arrays.fill(row, 1.0);
        }

        int id = 0;
        for (double[][] arg: args) {
            if (id == ignoreIndex) {
                id++;
                continue;
            }
            for (int i = 0; i < arg.length; i++) {
                for (int j = 0; j < arg[i].length; j++) {
                    res[i][j] *= arg[i][j];
                }
            }
            id++;
        }
        return res;
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
}