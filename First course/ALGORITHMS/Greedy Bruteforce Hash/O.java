import java.io.*;
import java.util.*;

/**
 * @author Michale Gerasimov
 * start: 13.06.2020
 * @version -
 */
public class O {

    public static void main(String[] args) throws IOException {
        FastReader scanner = new FastReader("multimap.in");
        PrintWriter writer = new PrintWriter("multimap.out");

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
                case "deleteall":
                    key = scanner.next();
                    set.removeAll(key);
                    break;
                case "get":
                    key = scanner.next();
                    String ans = set.get(key);
                    writer.println(Objects.requireNonNullElse(ans, "0"));
                    break;
                case "delete":
                    key = scanner.next();
                    value = scanner.next();
                    set.remove(key, value);
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


    static class MyMap {

        int SizeOfArray = 111;
        ArrayList<ArrayList<Pair>> array = new ArrayList<>();
        int size = 0;

        MyMap() {
            for (int i = 0; i < SizeOfArray; i++) {
                array.add(new ArrayList<>());
            }
        }

        void put(String key, String value) {
            int HashCode = hashCode(key);
            if (!containsKey(key)) {
                array.get(HashCode).add(new Pair(key, new MySet()));
                array.get(HashCode).get(array.get(HashCode).size() - 1).value.put(value);
                size++;
            } else {
                for (int i = 0; i < array.get(HashCode).size(); i++) {
                    if (array.get(HashCode).get(i).key.equals(key)) {
                        array.get(HashCode).get(i).value.put(value);
                        break;
                    }
                }
            }
        }

        String get(String  key) {
            if (size == 0) {
                return null;
            }

            int HashCode = hashCode(key);
            for (Pair pair: array.get(HashCode)) {
                if (pair.key.equals(key)) {
                    int ResSize = pair.value.size;
                    StringBuilder ResValues = new StringBuilder();
                    Node node = pair.value.LastNode;
                    while (true) {
                        ResValues.append(node.key);
                        ResValues.append(" ");
                        if (node.prev == null) {
                            break;
                        }
                        node = node.prev;
                    }
                    StringBuilder Result = new StringBuilder();
                    Result.append(ResSize);
                    Result.append(" ");
                    Result.append(ResValues);
                    return Result.toString();
                }
            }
            return null;
        }

        boolean containsKey(String key) {
            return get(key) != null;
        }

        void removeAll(String key) {
            if (containsKey(key)) {
                int HashCode = hashCode(key);
                for (int i = 0; i < array.get(HashCode).size(); i++) {
                    if (array.get(HashCode).get(i).key.equals(key)) {
                        array.get(HashCode).remove(i);
                        break;
                    }
                }
                size--;
            }
        }

        void remove(String key, String value) {
            if (containsKey(key)) {
                int HashCode = hashCode(key);
                for (int i = 0; i < array.get(HashCode).size(); i++) {
                    if (array.get(HashCode).get(i).key.equals(key)) {
                        array.get(HashCode).get(i).value.remove(value);

                        if (array.get(HashCode).get(i).value.size == 0) {
                            array.get(HashCode).remove(i);
                            size--;
                        }
                        break;
                    }
                }
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
        String key;
        MySet value;

        Pair(String key, MySet value) {
            this.key = key;
            this.value = value;
        }
    }

    static class MySet {

        int SizeOfArray = 113;
        ArrayList<ArrayList<Node>> array = new ArrayList<>();
        int size = 0;
        Node LastNode = null;
        Node CurrentNode = null;

        MySet() {
            for (int i = 0; i < SizeOfArray; i++) {
                array.add(new ArrayList<Node>());
            }
        }

        void put(String key) {
            if (!containsKey(key)) {
                CurrentNode = new Node(key);
                if (LastNode != null) {
                    LastNode.next = CurrentNode;
                }
                CurrentNode.prev = LastNode;
                LastNode = CurrentNode;
                array.get(hashCode(key)).add(CurrentNode);
                size++;
            }
        }

        String get(String  key) {
            if (size == 0) {
                return null;
            }

            for (Node node: array.get(hashCode(key))) {
                if (node.key.equals(key)) {
                    return node.key;
                }
            }
            return null;
        }

        boolean containsKey(String key) {
            return get(key) != null;
        }

        void remove(String key) {
            if (containsKey(key)) {
                int HashCode = hashCode(key);
                for (int i = 0; i < array.get(HashCode).size(); i++) {
                    if (array.get(HashCode).get(i).key.equals(key)) {
                        Node Current = array.get(HashCode).get(i);
                        if (Current.prev != null) {
                            Current.prev.next = Current.next;
                        }
                        if (Current.next != null) {
                            Current.next.prev = Current.prev;
                        }
                        if (LastNode.key.equals(Current.key)) {
                            LastNode = Current.prev;
                        }

                        array.get(HashCode).remove(i);
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

    static class Node {
        String key;
        Node next, prev;

        Node(String key) {
            this.key = key;
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

        void close() throws IOException {
            br.close();
        }
    }
}