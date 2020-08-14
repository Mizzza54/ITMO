package expression.operation;

import expression.TripleExpression;
import expression.generic.MyNumber;

/**
 * @author Michale Gerasimov
 * start: 10.03.2020
 * @version -
 */
public class Negate<T> extends UnaryOperation<T> {
    public Negate(TripleExpression<T> expression) {
        super(expression);
    }

    public T evaluate(T expression, MyNumber<T> op) {
        return op.negate(expression);
    }
}
