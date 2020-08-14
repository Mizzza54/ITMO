package expression.operation;

import expression.TripleExpression;
import expression.exceptions.ParserMyException;
import expression.generic.MyNumber;

/**
 * @author Michale Gerasimov
 * start: 10.12.2019
 * @version 10.12.2019
 */
public abstract class UnaryOperation<T> implements TripleExpression<T> {
    private final TripleExpression<T> expression;

    public UnaryOperation(TripleExpression<T> expression) {
        this.expression = expression;
    }

    public T evaluate(T x, T y, T z, MyNumber<T> op) throws ParserMyException {
        return evaluate(expression.evaluate(x, y, z, op), op);
    }

    public abstract T evaluate(T expression, MyNumber<T> op);
}
