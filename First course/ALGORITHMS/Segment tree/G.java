import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.StringTokenizer;

/**
 * @author Michale Gerasimov
 * start: 24.02.2020
 * @version -
 */
public class G {
    public static class MyComparator implements Comparator<Rectangle> {

        public int compare(Rectangle a, Rectangle b) {
            if (a.x == b.x) {
                if (a.side == true && b.side == true) {
                    return 0;
                } else if (a.side == true && b.side == false) {
                    return -1;
                } else if (a.side == false && b.side == true) {
                    return  1;
                } else {
                    return 0;
                }
            } else {
                if (a.x < b.x) {
                    return -1;
                } else {
                    return 1;
                }
            }
        }
    }

    public static class Node {
        public int max, index, add;

        Node(int max, int index, int add) {
            this.max = max;
            this.index = index;
            this.add = add;
        }
    }

    public static class Rectangle {
        public int x, yup, ydown;
        public boolean side;

        Rectangle(int x, int y1, int y2, boolean open) {
            this.x = x;
            this.yup = y1;
            this.ydown = y2;
            this.side = open;
        }
    }

    public static int n, sizeTree = 1 << 21;
    final static int shift = 200000;
    public static Node[] tree = new Node[2 * sizeTree - 1];;
    public static ArrayList<Rectangle> queue = new ArrayList<>();
    public static FastReader scanner = new FastReader(System.in);
    public static Comparator cmp = new MyComparator();

    public static void main(String[] args) {
        n = scanner.nextInt();
        int x1, y1, x2, y2;
        for (int i = 0; i < n; i++) {
            x1 = scanner.nextInt() + shift;
            y1 = scanner.nextInt() + shift;
            x2 = scanner.nextInt() + shift;
            y2 = scanner.nextInt() + shift;
            queue.add(new Rectangle(x1, y1, y2, true ));
            queue.add(new Rectangle(x2, y1, y2, false ));
        }

        Collections.sort(queue, cmp);

        for (int i = sizeTree - 1; i < 2 * sizeTree - 1; i++) {
            tree[i] = new Node(0, i - sizeTree + 1, 0);
        }
        for (int i = sizeTree - 2; i >= 0; i--) {
            tree[i] = new Node(0, tree[2 * i + 1].index, 0);
        }

        int x = 0, y = 0, max = 0;
        for (int i = 0; i < queue.size(); i++) {
            if (queue.get(i).side) {
                update(0, sizeTree - 1, 2 * sizeTree - 1, queue.get(i).yup + sizeTree - 1, queue.get(i).ydown + sizeTree, 1);
            }
            else {
                update(0, sizeTree - 1, 2 * sizeTree - 1, queue.get(i).yup + sizeTree - 1, queue.get(i).ydown + sizeTree, -1);
            }
            Node res = tree[0];
            if (res.max > max) {
                max = res.max;
                y = res.index - shift;
                x = queue.get(i).x - shift;
            }
        }

        System.out.println(max + "\n" + x + " " + y);
    }

    public static void push(int i) {
        tree[2 * i + 1].max += tree[i].add;
        tree[2 * i + 1].add += tree[i].add;
        tree[2 * i + 2].max += tree[i].add;
        tree[2 * i + 2].add += tree[i].add;
        tree[i].add = 0;
    }

    public static void update(int v, int l, int r, int a, int b, int x) {
        if (b <= l || r <= a) {
            return;
        }
        if (l >= a && r <= b) {
            tree[v].max += x;
            tree[v].add += x;
            return;
        }
        push(v);
        int m = (l + r) / 2;
        update(2 * v + 1, l, m, a, b, x);
        update(2 * v + 2, m, r, a, b, x);
        tree[v].max = Math.max(tree[2 * v + 1].max, tree[2 * v + 2].max);
        if (tree[2 * v + 1].max > tree[2 * v + 2].max) {
            tree[v].index = tree[2 * v + 1].index;
        } else {
            tree[v].index = tree[2 * v + 2].index;
        }
    }

    private static class FastReader {
        BufferedReader br;
        StringTokenizer st;

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
    }
}
