import java.io.*;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.StringTokenizer;
import java.util.ArrayList;


/**
 * @author Michael Gerasimov
 */
public class F {
    public static void main(String[] args) {
        FastReader scanner = new FastReader(System.in);
        PrintWriter writer = new PrintWriter(System.out);
        int m = scanner.nextInt();
        int k = scanner.nextInt();
        int h = scanner.nextInt();
        int n = scanner.nextInt();

        int[][] X = new int[n][m];
        int[] Y = new int[n];

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                X[i][j] = scanner.nextInt();
            }
            Y[i] = scanner.nextInt() - 1;
        }

        Method method = n < 1000 ? Method.FIRST : Method.SECOND;
        Tree tree = new Tree(X, Y, h, m, k, method);
        writer.println(tree.vertexCount);
        printTree(tree.root, writer);
        writer.flush();
        writer.close();
    }

    private static void printTree(Node root, PrintWriter writer) {
        if (root.isLeaf()) {
            writer.println(root);
        } else {
            writer.println(root);
            printTree(root.left, writer);
            printTree(root.right, writer);
        }
    }
}

class Tree {
    Node root;
    int maxDepth, m, k;
    int vertexCount = 0;

    Method method;

    public Tree(int[][] X, int[] Y, int maxDepth, int m, int k, Method method) {
        this.maxDepth = maxDepth;
        this.m = m;
        this.k = k;
        this.method = method;
        int[] map = new int[this.k];
        for (int y: Y) {
            map[y] += 1;
        }
        this.root = buildTree(X, Y, map, 0);
    }

    private Node buildTree(int[][] X, int[] Y, int[] map, int depth) {
        if (isFinish(depth, map)) {
            int mostCommonLabel = findMajorClass(map);
            vertexCount++;
            return new Node(mostCommonLabel, vertexCount);
        }

        Pair<int[], Integer>[] dataArray = new Pair[X.length];
        for (int i = 0; i < X.length; i++) {
            dataArray[i] = new Pair<>(X[i], Y[i]);
        }
        BestSplit bestSplit = findBestSplit(dataArray, map);

        if (bestSplit.leftX.length == 0 || bestSplit.rightX.length == 0) {
            int mostCommonLabel = findMajorClass(map);
            vertexCount++;
            return new Node(mostCommonLabel, vertexCount);
        } else {
            vertexCount++;
            int index = vertexCount;
            Node left = buildTree(bestSplit.leftX, bestSplit.leftY, bestSplit.leftMap, depth + 1);
            Node right = buildTree(bestSplit.rightX, bestSplit.rightY, bestSplit.rightMap, depth + 1);
            return new Node(bestSplit.feature, bestSplit.threshold, left, right, index);
        }
    }

    private int findMajorClass(int[] map) {
        int max = -1;
        int maxIndex = -1;
        for (int i = 0; i < map.length; i++) {
            if (map[i] > max) {
                max  = map[i];
                maxIndex = i;
            }
        }
        return maxIndex;
    }
    private boolean isFinish(int depth, int[] map) {
        int countNotZero = 0;
        for (int i : map) {
            countNotZero += i != 0 ? 1 : 0;
        }
        return depth >= this.maxDepth || countNotZero <= 1;
    }

    private double calculateEntropy(int[] map, int size) {
        double entropy = 0;
        switch (this.method) {
            case FIRST:
                for (int i = 0; i < this.k; i++) {
                    double value = map[i] / (double) size;
                    if (value > 0) {
                        entropy += value * (Math.log(value) / Math.log(2));
                    }
                }
                entropy = -entropy;
                break;
            case SECOND:
                for (int i = 0; i < this.k; i++) {
                    double value = map[i] / (double) size;
                    entropy += value * (1 - value);
                }
                break;
        }
        return entropy;
    }

    private Pair<Triplet<int[][], int[], int[]>, Triplet<int[][], int[], int[]>> buildSplit(Pair<int[], Integer>[] dataArray, double threshold, int feature) {
        List<int[]> leftX = new ArrayList<>();
        List<Integer> leftY = new ArrayList<>();
        int[] leftMap = new int[this.k];
        List<int[]> rightX = new ArrayList<>();
        List<Integer> rightY = new ArrayList<>();
        int[] rightMap = new int[this.k];

        for (Pair<int[], Integer> object: dataArray) {
            if (object.first[feature] <= threshold) {
                leftX.add(object.first);
                leftY.add(object.second);
                leftMap[object.second] += 1;
            } else {
                rightX.add(object.first);
                rightY.add(object.second);
                rightMap[object.second] += 1;
            }
        }

        return new Pair<>(
                new Triplet<>(
                        leftX.toArray(new int[leftX.size()][this.m]),
                        leftY.stream().mapToInt(i -> i).toArray(),
                        leftMap
                ),
                new Triplet<>(
                        rightX.toArray(new int[rightX.size()][this.m]),
                        rightY.stream().mapToInt(i -> i).toArray(),
                        rightMap
                )
        );
    }

    private double informationGain(double parentLoss,
                                   int size,
                                   int[] mapLeft,
                                   int sizeLeft,
                                   int[] mapRight,
                                   int sizeRight) {
        if (sizeLeft == 0 || sizeRight == 0) {
            return 0;
        } else {
            double childLoss = ((double) sizeLeft / size) * calculateEntropy(mapLeft, sizeLeft) + ((double) sizeRight / size) * calculateEntropy(mapRight, sizeRight);
            return parentLoss - childLoss;
        }
    }

    private BestSplit findBestSplit(Pair<int[], Integer>[] dataArray, int[] map) {
        double bestScore = Double.MIN_VALUE;
        double bestThreshold = -1.0;
        int bestFeature = -1;
        int prevThreshold = 0;
        double parentLoss = calculateEntropy(map, dataArray.length);


        for (int featureIndex = 0; featureIndex < this.m; featureIndex++) {

            final int index = featureIndex;
            Arrays.sort(dataArray, Comparator.comparingInt(o -> o.first[index]));

            if (dataArray[0].first[featureIndex] == dataArray[dataArray.length - 1].first[featureIndex]) {
                continue;
            }

            int[] mapLeft = new int[this.k];
            int sizeLeft = 0;
            int[] mapRight = Arrays.copyOf(map, map.length);
            int sizeRight = dataArray.length;


            for (Pair<int[], Integer> data: dataArray) {
                int threshold = data.first[featureIndex];
                if (threshold == prevThreshold) {
                    continue;
                }

                double score = informationGain(parentLoss, dataArray.length, mapLeft, sizeLeft, mapRight, sizeRight);
                if (score > bestScore) {
                    bestScore = score;
                    bestThreshold =  (double) (prevThreshold + threshold) / 2;
                    bestFeature = featureIndex;
                }
                prevThreshold = threshold;
                mapLeft[data.second] += 1;
                sizeLeft++;
                mapRight[data.second] -= 1;
                sizeRight--;
            }
        }
        Pair<Triplet<int[][], int[], int[]>, Triplet<int[][], int[], int[]>> temp = buildSplit(dataArray, bestThreshold, bestFeature);
        return new BestSplit(temp.first.first, temp.first.second, temp.first.third, temp.second.first, temp.second.second, temp.second.third, bestThreshold, bestFeature);
    }
}

enum Method {
    FIRST, SECOND
}

class Node {
    int feature, value;
    double threshold;
    Node left, right;
    boolean isLeaf;

    int index;

    public Node(int feature, double threshold, Node left, Node right, int index) {
        this.feature = feature;
        this.threshold = threshold;
        this.left = left;
        this.right = right;
        this.isLeaf = false;
        this.value = -1;
        this.index = index;
    }

    public Node(int value, int index) {
        this.value = value;
        this.isLeaf = true;
        this.feature = -1;
        this.threshold = -1;
        this.left = null;
        this.right = null;
        this.index = index;
    }

    public boolean isLeaf() {
        return this.isLeaf;
    }

    @Override
    public String toString() {
        if (this.isLeaf()) {
            return String.format("C %d", this.value + 1);
        } else {
            return String.format("Q %d %f %d %d", this.feature + 1, this.threshold, this.left.index, this.right.index);
        }
    }
}

class Pair<F, S> {
    public final F first;
    public final S second;

    public Pair(F first, S second) {
        this.first = first;
        this.second = second;
    }
}

class Triplet<F, S, T> {
    public final F first;
    public final S second;
    public final T third;

    public Triplet(F first, S second, T third) {
        this.first = first;
        this.second = second;
        this.third = third;
    }
}

class BestSplit {
    public final int[][] leftX;
    public final int[] leftY;
    public final int[] leftMap;
    public final int[][] rightX;
    public final int[] rightY;
    public final int[] rightMap;
    public final double threshold;
    public final int feature;

    public BestSplit(int[][] leftX, int[] leftY, int[] leftMap, int[][] rightX, int[] rightY, int[] rightMap, double threshold, int feature) {
        this.leftX = leftX;
        this.leftY = leftY;
        this.leftMap = leftMap;
        this.rightX = rightX;
        this.rightY = rightY;
        this.rightMap = rightMap;
        this.threshold = threshold;
        this.feature = feature;
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