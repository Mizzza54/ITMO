package expression;

import expression.generic.MyNumber;

import java.util.Objects;

/**
 * @author Michale Gerasimov
 * start: 02.12.2019
 * @version 10.12.2019
 */
public class Const<T> implements TripleExpression<T> {
    private final T value;

    public Const(T value) {
        this.value = value;
    }

    @Override
    public T evaluate(T x, T y, T z, MyNumber<T> op) {
        return value;
    }
}
