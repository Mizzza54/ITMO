package expression.operation;

import expression.TripleExpression;
import expression.exceptions.ParserMyException;
import expression.generic.MyNumber;

/**
 * @author Michale Gerasimov
 * start: 02.12.2019
 * @version 10.12.2019
 */
public abstract class BinaryOperation<T> implements TripleExpression<T> {
    private TripleExpression<T> left, right;

    public BinaryOperation(TripleExpression<T> left, TripleExpression<T> right) {
        this.left = left;
        this.right = right;
    }

    public T evaluate(T x, T y, T z, MyNumber<T> op) throws ParserMyException {
        return evaluate(left.evaluate(x, y, z, op), right.evaluate(x, y, z, op), op);
    }

    protected abstract T evaluate(T evaluate, T evaluate1, MyNumber<T> op);
}
