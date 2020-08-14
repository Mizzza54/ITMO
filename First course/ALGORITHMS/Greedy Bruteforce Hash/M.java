import java.io.*;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 05.06.2020
 * @version -
 */
public class M {

    public static void main(String[] args) throws IOException {
        FastReader scanner = new FastReader("map.in");
        PrintWriter writer = new PrintWriter("map.out");

        MyMap set = new MyMap();
        String cmd = scanner.next();
        String value, key;
        while (!cmd.equals("EOF")) {
            switch (cmd) {
                case "put":
                    key = scanner.next();
                    value = scanner.next();
                    set.put(key, value);
                    break;
                case "delete":
                    key = scanner.next();
                    set.remove(key);
                    break;
                case "get":
                    key = scanner.next();
                    String ans = set.get(key);
                    writer.println(Objects.requireNonNullElse(ans, "none"));
                    break;
            }

            cmd = scanner.next();
            if (cmd.equals("EOF")) {
                break;
            }
        }


        /*
        int cnt = 0;
        TreeSet<Integer> test = new TreeSet<>();
        RandomString random = new RandomString();
        for (int i = 0; i < 1000; i++) {
            int val = set.hashCode(random.getAlphaNumericString(10));
            if (test.contains(val)) {
                cnt++;
            }
            test.add(val);
        }
        System.out.println(cnt);

         */

        scanner.close();
        writer.close();
    }

    static class RandomString {
        static String getAlphaNumericString(int n) {
            String AlphaNumericString = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" + "0123456789" + "abcdefghijklmnopqrstuvxyz";
            StringBuilder sb = new StringBuilder(n);
            for (int i = 0; i < n; i++) {
                int index = (int)(AlphaNumericString.length() * Math.random());
                sb.append(AlphaNumericString.charAt(index));


            }
            return sb.toString();
        }
    }

    static class MyMap{

        int SizeOfArray = 500000;
        int Prime = 3571;
        LinkedList<Pair>[] array = new LinkedList[SizeOfArray];
        int size = 0;

        MyMap() {
            for (int i = 0; i < SizeOfArray; i++) {
                array[i] = new LinkedList<Pair>();
            }
        }

        void put(String key, String value) {
            if (!containsKey(key)) {
                array[hashCode(key)].add(new Pair(key, value));
                size++;
            } else {
                for (int i = 0; i < array[hashCode(key)].size(); i++) {
                    if (array[hashCode(key)].get(i).key.equals(key)) {
                        array[hashCode(key)].set(i, new Pair(key, value));
                        break;
                    }
                }
            }
        }

        String get(String  key) {
            if (size == 0) {
                return null;
            }

            for (Pair pair: array[hashCode(key)]) {
                if (pair.key.equals(key)) {
                    return pair.value;
                }
            }
            return null;
        }

        boolean containsKey(String key) {
            return get(key) != null;
        }

        void remove(String key) {
            if (containsKey(key)) {
                for (int i = 0; i < array[hashCode(key)].size(); i++) {
                    if (array[hashCode(key)].get(i).key.equals(key)) {
                        array[hashCode(key)].remove(i);
                        break;
                    }
                }
                size--;
            }
        }

        int hashCode(String key) {
            int hash = 0;
            byte[] bytes = key.getBytes();
            for (byte v : bytes) {
                hash = 31 * hash + (v & 0xff);
            }
            hash = Math.abs(hash);
            return hash % SizeOfArray;
        }
    }

    static class Pair {
        String key, value;

        Pair() {
            this.key = null;
            this.value = null;
        }

        Pair(String key, String value) {
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
