package expression.generic;

/**
 * @author Michale Gerasimov
 * start: 10.03.2020
 * @version -
 */
public abstract class MyNumber<T> {

public abstract T add(T a, T b);

public abstract T subtract(T a, T b);

public abstract T divide(T a, T b);

public abstract T multiply(T a, T b);

public abstract T negate(T a);

public abstract T parseToDigit(String str);

public abstract T parseToDigitTab(String str);
}