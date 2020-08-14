package expression;

import expression.exceptions.ParserMyException;
import expression.generic.MyNumber;
import test.ToMiniString;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface TripleExpression<T> extends ToMiniString {
    T evaluate(T x, T y, T z, MyNumber<T> op) throws ParserMyException;
}