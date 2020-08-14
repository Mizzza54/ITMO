import java.io.*;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 04.06.2020
 * @version -
 */
public class L {

    public static void main(String[] args) throws IOException {
        FastReader scanner = new FastReader("set.in");
        PrintWriter writer = new PrintWriter("set.out");

        MyMap set = new MyMap();
        String string = scanner.next();
        int value = scanner.nextInt();
        while (!string.equals("EOF")) {
            switch (string) {
                case "insert":
                    set.put(value, 1);
                    break;
                case "delete":
                    set.remove(value);
                    break;
                case "exists":
                    writer.println(set.containsKey(value));
                    break;
            }

            string = scanner.next();
            if (string.equals("EOF")) {
                break;
            }
            value = scanner.nextInt();
        }

        scanner.close();
        writer.close();
    }

    static class MyMap{

        int SizeOfArray = 209000;
        int Prime = 655360001;
        LinkedList<Pair>[] array = new LinkedList[SizeOfArray];
        int size = 0;

        MyMap() {
            for (int i = 0; i < SizeOfArray; i++) {
                array[i] = new LinkedList<Pair>();
            }
        }

        void put(int key, int value) {
            if (!containsKey(key)) {
                array[hashCode(key)].add(new Pair(key, value));
                size++;
            }
        }

        Integer get(int key) {
            if (size == 0) {
                return null;
            }

            for (Pair pair: array[hashCode(key)]) {
                if (pair.key == key) {
                    return pair.value;
                }
            }
            return null;
        }

        boolean containsKey(int key) {
            return get(key) != null;
        }

        void remove(int key) {
            if (containsKey(key)) {
                for (int i = 0; i < array[hashCode(key)].size(); i++) {
                    if (array[hashCode(key)].get(i).key == key) {
                        array[hashCode(key)].remove(i);
                        break;
                    }
                }
                size--;
            }
        }

        int hashCode(int key) {
            return (Math.abs(key)) % SizeOfArray;
        }
    }

    static class Pair {
        Integer key, value;

        Pair() {
            this.key = null;
            this.value = null;
        }

        Pair(int key, int value) {
            this.key = key;
            this.value = value;
        }
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
                } catch (Exception e) {
                    return "EOF";
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
