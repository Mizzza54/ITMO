package info.kgeorgiy.ja.gerasimov.crawler;

/**
 * @author Michael Gerasimov
 * start: 02.05.2021
 * @version -
 */
public class Node {
    String url;
    int depth;

    Node(String url, int depth) {
        this.url = url;
        this.depth = depth;
    }

    public String getUrl() {
        return url;
    }

    public int getDepth() {
        return depth;
    }
}
