package operation;

import expression.CommonExpression;
import operation.BinaryOperation;

/**
 * @author Michale Gerasimov
 * start: 02.12.2019
 * @version 07.12.2019
 */
public class Subtract extends BinaryOperation {

    public Subtract(CommonExpression left, CommonExpression right) {
        super(left, right);
    }

    public int consider(int left, int right) {
        return left - right;
    }

    public double consider(double left, double right) {
        return left - right;
    }

    public char getOperator() {
        return '-';
    }
}
