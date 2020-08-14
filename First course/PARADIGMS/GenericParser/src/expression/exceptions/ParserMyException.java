package expression.exceptions;

/**
 * @author Michale Gerasimov
 * start: 05.02.2020
 * @version -
 */
public class ParserMyException extends Exception {
    public ParserMyException(String s, int index, int len) {
        super(mark(s,index, len));
    }

    public static String mark(String s, int index, int len) {
        StringBuilder sb = new StringBuilder(s + "\n");
        for (int i = 0; i < s.length() + 50 - len + index; i++) {
            sb.append(" ");
        }
        sb.append("^");
        s = sb.toString();
        return s;
    }

}
