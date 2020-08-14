package operation.check;

import expression.CommonExpression;
import expression.exceptions.ArithmeticMyException;
import operation.BinaryOperation;

/**
 * @author Michale Gerasimov
 * start: 04.02.2020
 * @version -
 */
public class CheckedDivide extends BinaryOperation {
    public CheckedDivide(CommonExpression left, CommonExpression right) {
        super(left, right);
    }

    public int consider(int left, int right) throws ArithmeticMyException {
        if (right == 0) {
            throw new ArithmeticMyException("Division by zero : " + left + " / " + right);
        }
        if (left == Integer.MIN_VALUE && right == -1) {
            throw new ArithmeticMyException("Overflow exception : " + left + " / " + right);
        }
        return left / right;
    }

    public double consider(double left, double right) {
        return left / right;
    }

    public char getOperator() {
        return '/';
    }
}
