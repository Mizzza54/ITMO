package operation;

import expression.CommonExpression;
import operation.BinaryOperation;

/**
 * @author Michale Gerasimov
 * start: 17.12.2019
 * @version -
 */
public class ShiftLeft extends BinaryOperation {

    public ShiftLeft(CommonExpression left, CommonExpression right) {
        super(left, right);
    }

    @Override
    public int consider(int left, int right) {
        return left << right;
    }

    @Override
    public double consider(double left, double right) {
        return 0;
    }

    @Override
    public char getOperator() {
        return '<';
    }
}
