package expression.operation;


import expression.TripleExpression;
import expression.generic.MyNumber;

/**
 * @author Michale Gerasimov
 * start: 02.12.2019
 * @version 07.12.2019
 */
public class Add<T> extends BinaryOperation<T> {

    public Add(TripleExpression<T> left, TripleExpression<T> right) {
        super(left, right);
    }

    @Override
    protected T evaluate(T left, T right, MyNumber<T> op) {
        return op.add(left, right);
    }
}
