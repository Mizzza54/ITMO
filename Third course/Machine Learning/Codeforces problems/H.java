import java.io.*;
import java.util.*;


/**
 * @author Michael Gerasimov
 */
public class H {
    private static final FastReader scanner = new FastReader(System.in);
    private static final PrintWriter writer = new PrintWriter(System.out);

    public static void main(String[] args) {
        int n, m, k;
        n = scanner.nextInt();
        m = scanner.nextInt();
        k = scanner.nextInt();

        List<AbstractNode> nodes = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            String type = scanner.next();
            AbstractNode node = null;
            switch (type) {
                case "var":
                    int r = scanner.nextInt();
                    int c = scanner.nextInt();
                    node = new VarNode(r, c);
                    break;
                case "tnh":
                    int xt = scanner.nextInt() - 1;
                    node = new TnhNode(xt);
                    break;
                case "rlu":
                    int alpha = scanner.nextInt();
                    int xr = scanner.nextInt() - 1;
                    node = new RluNode(alpha, xr);
                    break;
                case "mul":
                    int a = scanner.nextInt() - 1;
                    int b = scanner.nextInt() - 1;
                    node = new MulNode(a, b);
                    break;
                case "sum":
                    int lens = scanner.nextInt();
                    List<Integer> us = new ArrayList<>();
                    for (int j = 0; j < lens; j++) {
                        us.add(scanner.nextInt() - 1);
                    }
                    node = new SumNode(us);
                    break;
                case "had":
                    int lenh = scanner.nextInt();
                    List<Integer> uh = new ArrayList<>();
                    for (int j = 0; j < lenh; j++) {
                        uh.add(scanner.nextInt() - 1);
                    }
                    node = new HadNode(uh);
                    break;
            }
            nodes.add(node);
        }

        for (int i = 0; i < m; i++) {
            ((VarNode) nodes.get(i)).readData(scanner);
            nodes.get(i).forwardPass();
        }

        for (int i = m; i < n; i++) {
            nodes.get(i).readData(nodes);
            nodes.get(i).forwardPass();
        }

        for (int i = n - k; i < n; i++) {
            nodes.get(i).readDerivative(scanner);
        }

        for (int i = n - 1; i > -1; i--) {
            nodes.get(i).backwardsPass();
            nodes.get(i).giveBackwardsData(nodes);
        }

        for (int i = n - k; i < n; i++) {
            printMatrix(nodes.get(i).outputData);
        }

        for (int i = 0; i < m; i++) {
            printMatrix(nodes.get(i).derivative);
        }

        writer.flush();
        writer.close();
    }

    private static void printMatrix(double[][] matrix) {
        for (double[] line : matrix) {
            for (double elem : line) {
                writer.print(elem + " ");
            }
            writer.println();
        }
    }
}

abstract class AbstractNode {
    protected double[][] outputData;

    protected double[][] derivative;

    protected AbstractNode() {}

    abstract protected void forwardPass();

    abstract protected void readData(List<AbstractNode> nodes);

    abstract protected void backwardsPass();

    abstract protected void giveBackwardsData(List<AbstractNode> nodes);

    protected void readDerivative(FastReader scanner) {
        for (int i = 0; i < outputData.length; i++) {
            for (int j = 0; j < outputData[0].length; j++) {
                derivative[i][j] = scanner.nextInt();
            }
        }
    }

    @Override
    public String toString() {
        return "AbstractNode{" +
                "outputData=" + Arrays.deepToString(outputData) +
                ", derivative=" + Arrays.deepToString(derivative) +
                '}';
    }

    protected double[][] addingMatrix(double[][] a, double[][] b) {
        double[][] res = new double[a.length][a[0].length];
        for (int i = 0; i < a.length; i++) {
            for (int j = 0; j < a[i].length; j++) {
                res[i][j] = a[i][j] + b[i][j];
            }
        }
        return res;
    }

    protected double[][] copy(double[][] src) {
        if (src == null) {
            return null;
        }

        double[][] copy = new double[src.length][];
        for (int i = 0; i < src.length; i++) {
            copy[i] = src[i].clone();
        }

        return copy;
    }
}

abstract class UnaryFunctionNode extends AbstractNode {
    protected final int argIndex;
    protected double[][] arg;
    protected double[][] backwardsData;

    public UnaryFunctionNode(int argIndex) {
        super();
        this.argIndex = argIndex;
    }

    @Override
    protected void readData(List<AbstractNode> nodes) {
        arg = nodes.get(argIndex).outputData;
        outputData = new double[this.arg.length][this.arg[0].length];
        backwardsData = new double[this.arg.length][this.arg[0].length];
        derivative = new double[outputData.length][outputData[0].length];
    }

    @Override
    protected void giveBackwardsData(List<AbstractNode> nodes) {
        if (nodes.get(argIndex).derivative.length != backwardsData.length || nodes.get(argIndex).derivative[0].length != backwardsData[0].length) {
            throw new IllegalStateException();
        }

        nodes.get(argIndex).derivative = addingMatrix(backwardsData, nodes.get(argIndex).derivative);
    }
}

abstract class BinaryFunctionNode extends AbstractNode {
    protected final int leftArgIndex, rightArgIndex;
    protected double[][] leftArg, rightArg;
    protected double[][] leftBackwardsData, rightBackwardsData;

    public BinaryFunctionNode(int leftArgIndex, int rightArgIndex) {
        super();
        this.leftArgIndex = leftArgIndex;
        this.rightArgIndex = rightArgIndex;
    }

    @Override
    protected void readData(List<AbstractNode> nodes) {
        leftArg = nodes.get(leftArgIndex).outputData;
        rightArg = nodes.get(rightArgIndex).outputData;
        outputData = new double[leftArg.length][rightArg[0].length];
        derivative = new double[outputData.length][outputData[0].length];
    }

    @Override
    protected void giveBackwardsData(List<AbstractNode> nodes) {
        if (nodes.get(leftArgIndex).derivative.length != leftBackwardsData.length || nodes.get(leftArgIndex).derivative[0].length != leftBackwardsData[0].length) {
            throw new IllegalStateException();
        }

        if (nodes.get(rightArgIndex).derivative.length != rightBackwardsData.length || nodes.get(rightArgIndex).derivative[0].length != rightBackwardsData[0].length) {
            throw new IllegalStateException();
        }

        nodes.get(leftArgIndex).derivative = addingMatrix(leftBackwardsData, nodes.get(leftArgIndex).derivative);
        nodes.get(rightArgIndex).derivative = addingMatrix(rightBackwardsData, nodes.get(rightArgIndex).derivative);
    }
}

abstract class VariadicFunctionsNode extends AbstractNode {
    protected final List<Integer> argsIndices;
    protected List<double[][]> args;

    protected List<double[][]> backwardsData;

    public VariadicFunctionsNode(List<Integer> argsIndices) {
        super();
        this.argsIndices = argsIndices;
        args = new ArrayList<>();
        backwardsData = new ArrayList<>();
    }

    @Override
    protected void readData(List<AbstractNode> nodes) {
        for (Integer id: argsIndices) {
            args.add(nodes.get(id).outputData);
        }
        outputData = new double[args.get(0).length][args.get(0)[0].length];
        derivative = new double[outputData.length][outputData[0].length];
    }

    @Override
    protected void giveBackwardsData(List<AbstractNode> nodes) {
        int i = 0;
        for (Integer id: argsIndices) {
            if (nodes.get(id).derivative.length != backwardsData.get(i).length || nodes.get(id).derivative[0].length != backwardsData.get(i)[0].length) {
                throw new IllegalStateException();
            }

            nodes.get(id).derivative = addingMatrix(backwardsData.get(i), nodes.get(id).derivative);
            i++;
        }
    }
}

class VarNode extends AbstractNode {
    protected double[][] arg;
    public final int r, c;

    protected VarNode(int r, int c) {
        super();
        this.r = r;
        this.c = c;
        this.arg = new double[r][c];
    }

    @Override
    protected void forwardPass() {
        this.outputData = copy(arg);
        derivative = new double[outputData.length][outputData[0].length];
    }

    @Override
    protected void readData(List<AbstractNode> nodes) {}

    @Override
    protected void backwardsPass() {

    }

    @Override
    protected void giveBackwardsData(List<AbstractNode> nodes) {

    }

    protected void readData(final FastReader scanner) {
        for (int i = 0; i < r; i++) {
            for (int j = 0; j < c; j++) {
                arg[i][j] = scanner.nextInt();
            }
        }
    }
}

class TnhNode extends UnaryFunctionNode {
    protected TnhNode(int x) {
        super(x);
    }

    @Override
    protected void forwardPass() {
        for (int i = 0; i < this.arg.length; i++) {
            for (int j = 0; j < this.arg[i].length; j++) {
                this.outputData[i][j] = Math.tanh(this.arg[i][j]);
            }
        }
    }

    @Override
    protected void backwardsPass() {
        for (int i = 0; i < arg.length; i++) {
            for (int j = 0; j < arg[i].length; j++) {
                backwardsData[i][j] = derivative[i][j] * (1 - Math.pow(Math.tanh(arg[i][j]), 2));
            }
        }
    }
}

class RluNode extends UnaryFunctionNode {
    private final int alpha;

    protected RluNode(int alpha, int x) {
        super(x);
        this.alpha = alpha;
    }

    @Override
    protected void forwardPass() {
        double coefficient = 1.0 / this.alpha;

        for (int i = 0; i < this.arg.length; i++) {
            for (int j = 0; j < this.arg[i].length; j++) {
                outputData[i][j] = this.arg[i][j] < 0 ? coefficient * this.arg[i][j] : this.arg[i][j];
            }
        }
    }

    @Override
    protected void backwardsPass() {
        double coefficient = 1.0 / this.alpha;

        for (int i = 0; i < arg.length; i++) {
            for (int j = 0; j < arg[i].length; j++) {
                backwardsData[i][j] = derivative[i][j] * (this.arg[i][j] < 0 ? coefficient : 1);
            }
        }
    }
}

class MulNode extends BinaryFunctionNode {
    protected MulNode(int a, int b) {
        super(a, b);
    }

    @Override
    protected void forwardPass() {
        outputData = multiplyMatrix(leftArg, rightArg);
    }

    @Override
    protected void backwardsPass() {
        leftBackwardsData = multiplyMatrix(derivative, transposeMatrix(rightArg));
        rightBackwardsData = multiplyMatrix(transposeMatrix(leftArg), derivative);
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

class SumNode extends VariadicFunctionsNode {
    protected SumNode(List<Integer> u) {
        super(u);
    }

    @Override
    protected void forwardPass() {
        for (double[][] arg: this.args) {
            for (int i = 0; i < arg.length; i++) {
                for (int j = 0; j < arg[i].length; j++) {
                    this.outputData[i][j] += arg[i][j];
                }
            }
        }
    }

    @Override
    protected void backwardsPass() {
        for (int i = 0; i < args.size(); i++) {
            backwardsData.add(copy(derivative));
        }
    }
}

class HadNode extends VariadicFunctionsNode {
    protected HadNode(List<Integer> u) {
        super(u);
    }

    @Override
    protected void forwardPass() {
        outputData = hadamardProduct(args, -1);
    }

    @Override
    protected void backwardsPass() {
        for (int k = 0; k < args.size(); k++) {
            double[][] temp = hadamardProduct(args, k);

            for (int i = 0; i < temp.length; i++) {
                for (int j = 0; j < temp[i].length; j++) {
                    temp[i][j] *= derivative[i][j];
                }
            }

            backwardsData.add(temp);
        }
    }

    private double[][] hadamardProduct(List<double[][]> args, int ignoreIndex) {
        double[][] res = new double[args.get(0).length][args.get(0)[0].length];

        if (args.size() == 2 && (ignoreIndex == 0 || ignoreIndex == 1)) {
            res = copy(args.get(ignoreIndex == 0 ? 1 : 0));
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