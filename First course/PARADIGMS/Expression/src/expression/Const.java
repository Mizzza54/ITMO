package expression;

import java.util.Objects;

/**
 * @author Michale Gerasimov
 * start: 02.12.2019
 * @version 10.12.2019
 */
public class Const implements CommonExpression {
    private final double value;

    public Const(double value) {
        this.value = value;
    }

    public Const(int value) {
        this.value = value;
    }

    public int evaluate(int x) {
        return (int) value;
    }

    public String toString() {
        String str = String.valueOf(value);
        if (value % 1 == 0) {
            int tmp = (int) value;
            return String.valueOf(tmp);
        } else {
            return str;
        }
    }

    public double evaluate(double x) {
        return value;
    }

    @Override
    public boolean equals(Object object) {
        if (object == null || getClass() != object.getClass()) {
            return false;
        }
        return value == ((Const)object).value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return (int) value;
    }
}
