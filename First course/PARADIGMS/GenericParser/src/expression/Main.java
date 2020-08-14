package expression;

import expression.exceptions.ParserMyException;
import expression.generic.MyNumber;
import expression.generic.numbers.IntegerNumbers;
import expression.parser.ExpressionParser;

/**
 * @author Michale Gerasimov
 * start: 03.12.2019
 * @version -
 */
public class Main {

    public static void main(String[] args) throws ParserMyException {
        IntegerNumbers op = new IntegerNumbers();
        ExpressionParser<Integer> parser = new ExpressionParser<Integer>(op);
        String str = "10";
        System.out.println(parser.parse(str));
    }
}
