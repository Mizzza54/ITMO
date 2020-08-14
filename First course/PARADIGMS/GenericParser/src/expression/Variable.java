package expression;

import expression.exceptions.ParserMyException;
import expression.generic.MyNumber;

/**
 * @author Michale Gerasimov
 * start: 02.12.2019
 * @version 07.12.2019
 */
public class Variable<T> implements TripleExpression<T> {
    private final String name;

    public Variable(String name) {
        this.name = name;
    }

    @Override
    public T  evaluate(T x, T y, T z, MyNumber<T> op) throws ParserMyException {
        switch (name) {
            case "x":
                return x;
            case "y":
                return y;
            case "z":
                return z;
            default:
                throw new ParserMyException("ERROR in Variable", 0, 1);
        }
    }
}
