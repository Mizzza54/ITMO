package G;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;

/**
 * @author Michael Gerasimov
 * start: 19.12.2020
 * @version -
 */
public class G {
    public static void main(String[] args) throws IOException {
        Tree Bor = new Tree();
        Bor.SuffixBor.add(new Vertex(-1, -1));
        //Bor.SuffixBor.add(new Vertex());
        FastScanner scanner = new FastScanner();
        int n = scanner.nextInt();
        for (int i = 0; i < n; i++) {
            Bor.addString(scanner.nextString(), i + 1);
        }

        boolean[] answer = Bor.processText(scanner.nextString(), n);

        PrintWriter writer = new PrintWriter(System.out);
        for (boolean bool : answer) {
            if (bool) {

                writer.println("YES");
            } else {
                writer.println("NO");
            }
        }
        writer.close();
    }
}

class Vertex {
    int[] son = new int[26];                                    // массив сыновей
    int[] go = new int[26];                                     // массив переходов (запоминаем переходы в ленивой рекурсии), используемый для вычисления суффиксных ссылок
    int parent;                                                 // вершина родитель
    int suffixLink;                                             // суффиксная ссылка (вычисляем в ленивой рекурсии)
    int charToParent;                                           // символ, ведущий к родителю
    boolean isLeaf;                                             // флаг, является ли вершина терминалом
    ArrayList<Integer> leafPatternNumber = new ArrayList<>();   // номера строк, за которые отвечает терминал

    public Vertex(int parent, int charToParent) {
        Arrays.fill(this.son, -1);
        Arrays.fill(this.go, -1);
        this.parent = parent;
        this.charToParent = charToParent;
        this.suffixLink = -1;
    }
}

class Tree {
    public ArrayList<Vertex> SuffixBor = new ArrayList<Vertex>();

    public int getSuffLink(int index) {
        Vertex vertex = SuffixBor.get(index);
        if (vertex.suffixLink == -1) {
            if (index == -1 || vertex.parent == 0) {
                vertex.suffixLink = 0;
            } else {
                vertex.suffixLink = getLink(getSuffLink(vertex.parent), vertex.charToParent);
            }
        }
        return vertex.suffixLink;
    }

    public  int getLink(int index, int c) {
        Vertex vertex = SuffixBor.get(index);
        if (vertex.go[c] == -1) {
            if (vertex.son[c] != -1) {
                vertex.go[c] = vertex.son[c];
            } else if (index == 0) {
                vertex.go[c] = 0;
            } else {
                vertex.go[c] = getLink(getSuffLink(index), c);
            }
        }
        return vertex.go[c];
    }

    public void addString(String s, int NumOfString) {
        int index = 0;
        for (char ch_raw: s.toCharArray()) {
            int ch = ch_raw - 'a';

            if (SuffixBor.get(index).son[ch] == -1) {
                SuffixBor.get(index).son[ch] = SuffixBor.size();
                SuffixBor.add(new Vertex(index, ch));
            }
            index = SuffixBor.get(index).son[ch];
        }
        SuffixBor.get(index).isLeaf = true;
        SuffixBor.get(index).leafPatternNumber.add(NumOfString);
    }

    public boolean[] processText(String text, int n) {
        boolean[] answer = new boolean[n];
        boolean[] visited = new boolean[SuffixBor.size()];
        int index = 0;

        for (char ch_raw : text.toCharArray()) {
            int ch = ch_raw - 'a';
            index = getLink(index, ch);
            Vertex vertex = SuffixBor.get(index);

            int temp = index;
            while (temp != 0 && !visited[temp]) {
                visited[temp] = true;
                if (vertex.isLeaf) {
                    for (int i : vertex.leafPatternNumber) {
                        answer[i - 1] = true;
                    }
                }
                temp = getSuffLink(temp);
                vertex = SuffixBor.get(temp);
            }
        }

        return  answer;
    }
}

class FastScanner {
    public StreamTokenizer t;

    FastScanner() {
        t = new StreamTokenizer( new BufferedReader( new InputStreamReader(System.in)));
    }

    public int nextInt() throws IOException {
        t.nextToken();
        return (int) t.nval;
    }

    public long nextLong() throws IOException {
        t.nextToken();
        return (long) t.nval;
    }

    public String nextString() throws IOException {
        t.nextToken();
        return t.sval;
    }
}