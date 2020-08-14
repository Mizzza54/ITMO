package operation;

import expression.CommonExpression;
import expression.exceptions.ArithmeticMyException;

import java.util.Objects;

/**
 * @author Michale Gerasimov
 * start: 02.12.2019
 * @version 10.12.2019
 */
public abstract class BinaryOperation implements CommonExpression {
    private CommonExpression left, right;

    public BinaryOperation(CommonExpression left, CommonExpression right) {
        this.left = left;
        this.right = right;
    }

    abstract public int consider(int left, int right);

    abstract public double consider(double left, double right);

    public double evaluate(double x) {
        return consider(left.evaluate(x), right.evaluate(x));
    }

    public int evaluate(int x) {
        return consider(left.evaluate(x), right.evaluate(x));
    }

    public abstract char getOperator();

    public int evaluate(int x, int y, int z) throws ArithmeticMyException {
        return consider(left.evaluate(x, y, z), right.evaluate(x, y, z));
    }

    public String toString() {
        return "(" + left.toString() + " " + getOperator() + " " + right.toString() +")";
    }

    public boolean equals(Object object) {
        if (object == null || object.getClass() != getClass()) {
            return false;
        }
        return left.equals(((BinaryOperation) object).left) && right.equals(((BinaryOperation) object).right);
    }

    @Override
    public int hashCode() {
        return left.hashCode() * (228 + Objects.hashCode(getOperator()) * 322) + right.hashCode() * 54;
    }
}
