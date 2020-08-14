package operation.check;

import expression.CommonExpression;
import expression.exceptions.ArithmeticMyException;
import operation.BinaryOperation;

/**
 * @author Michale Gerasimov
 * start: 04.02.2020
 * @version -
 */
public class CheckedMultiply extends BinaryOperation {
    public CheckedMultiply(CommonExpression left, CommonExpression right) {
        super(left, right);
    }

    public int consider(int left, int right) throws ArithmeticMyException {
        if (left > 0 && right > 0 && Integer.MAX_VALUE / left < right) {
            throw new ArithmeticMyException("Overflow exception : " + left + " * " + right);
        }
        if (left > 0 && right < 0 && Integer.MIN_VALUE / left > right) {
            throw new ArithmeticMyException("Overflow exception : " + left + " * " + right);
        }
        if (left < 0 && right > 0 && Integer.MIN_VALUE / right > left) {
            throw new ArithmeticMyException("Overflow exception : " + left + " * " + right);
        }
        if (left < 0 && right < 0 && Integer.MAX_VALUE / left > right) {
            throw new ArithmeticMyException("Overflow exception : " + left + " * " + right);
        }
        return left * right;
    }

    public double consider(double left, double right) {
        return left * right;
    }

    public char getOperator() {
        return '*';
    }
}
