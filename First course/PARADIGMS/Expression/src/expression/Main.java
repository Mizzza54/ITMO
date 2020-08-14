package expression;

import expression.exceptions.ArithmeticMyException;
import expression.exceptions.ParserMyException;
import expression.parser.ExpressionParser;
import expression.parser.Parser;

/**
 * @author Michale Gerasimov
 * start: 03.12.2019
 * @version -
 */
public class Main {
    public static void main(String[] args) throws ParserMyException {
        Parser test = new ExpressionParser();
        try {
            //System.out.println("test: " + 2147483648l);
            System.out.println("Integer.MIN_VALUE = " + Integer.MIN_VALUE);
            System.out.println("Integer.MAX_VALUE = " + Integer.MAX_VALUE);
            System.out.println(test.parse(Long.toString((-(-2147483648L)))).evaluate(5));
        } catch (ArithmeticMyException e) {
            e.printStackTrace();
        }
    }
}
