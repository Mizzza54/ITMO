import java.io.*;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 05.06.2020
 * @version -
 */
public class N {

    public static void main(String[] args) throws IOException {
        FastReader scanner = new FastReader("linkedmap.in");
        PrintWriter writer = new PrintWriter("linkedmap.out");

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
                case "prev":
                    key = scanner.next();
                    writer.println(set.prev(key));
                    break;
                case "next":
                    key = scanner.next();
                    writer.println(set.next(key));
                    break;
            }

            cmd = scanner.next();
            if (cmd.equals("EOF")) {
                break;
            }
        }

        scanner.close();
        writer.close();
    }


    static class MyMap{

        int SizeOfArray = 500000;
        LinkedList<Node>[] array = new LinkedList[SizeOfArray];
        int size = 0;
        Node LastNode = null;
        Node CurrentNode = null;

        MyMap() {
            for (int i = 0; i < SizeOfArray; i++) {
                array[i] = new LinkedList<Node>();
            }
        }

        void put(String key, String value) {
            if (!containsKey(key)) {
                CurrentNode = new Node(key, value);
                if (LastNode != null) {
                    LastNode.next = CurrentNode;
                }
                CurrentNode.prev = LastNode;
                LastNode = CurrentNode;
                array[hashCode(key)].add(CurrentNode);
                size++;
            } else {
                for (int i = 0; i < array[hashCode(key)].size(); i++) {
                    if (array[hashCode(key)].get(i).key.equals(key)) {
                        Node Current = new Node(key, value);
                        if (array[hashCode(key)].get(i).prev != null) {
                            Current.prev = array[hashCode(key)].get(i).prev;
                        }
                        if ( array[hashCode(key)].get(i).next != null) {
                            Current.next = array[hashCode(key)].get(i).next;
                        }

                        if (Current.prev != null) {
                            Current.prev.next = Current;
                        }
                        if (Current.next != null) {
                            Current.next.prev = Current;
                        }

                        if (LastNode.key.equals(Current.key)) {
                            LastNode = Current;
                        }
                        array[hashCode(key)].set(i, Current);
                        break;
                    }
                }
            }
        }

        String get(String  key) {
            if (size == 0) {
                return null;
            }

            for (Node node: array[hashCode(key)]) {
                if (node.key.equals(key)) {
                    return node.value;
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
                        Node Current = array[hashCode(key)].get(i);
                        if (Current.prev != null) {
                            Current.prev.next = Current.next;
                        }
                        if (Current.next != null) {
                            Current.next.prev = Current.prev;
                        }
                        if (LastNode.key.equals(Current.key)) {
                            LastNode = Current.prev;
                        }

                        array[hashCode(key)].remove(i);
                        break;
                    }
                }
                size--;
            }
        }

        String next(String key) {
            Node Result;
            if (containsKey(key)) {
                for (int i = 0; i < array[hashCode(key)].size(); i++) {
                    if (array[hashCode(key)].get(i).key.equals(key)) {
                        Result = array[hashCode(key)].get(i);
                        if (Result.next != null) {
                            return Result.next.value;
                        } else {
                            return "none";
                        }
                    }
                }
            }
            return "none";
        }

        String prev(String key) {
            Node Result;
            if (containsKey(key)) {
                for (int i = 0; i < array[hashCode(key)].size(); i++) {
                    if (array[hashCode(key)].get(i).key.equals(key)) {
                        Result = array[hashCode(key)].get(i);
                        if (Result.prev != null) {
                            return Result.prev.value;
                        } else {
                            return "none";
                        }
                    }
                }
            }
            return "none";
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

    static class Node {
        String key, value;
        Node next, prev;

        Node(String key, String value) {
            this.key = key;
            this.value = value;
            this.next = null;
            this.prev = null;
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
