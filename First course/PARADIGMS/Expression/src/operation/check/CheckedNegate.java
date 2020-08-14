package operation.check;

import expression.CommonExpression;
import expression.exceptions.ArithmeticMyException;
import operation.UnaryOperation;

/**
 * @author Michale Gerasimov
 * start: 04.02.2020
 * @version -
 */
public class CheckedNegate extends UnaryOperation {
    public CheckedNegate(CommonExpression expression) {
        super(expression);
    }

    @Override
    public int consider(int expression) throws ArithmeticMyException {
        if (expression < Integer.MIN_VALUE)
            throw new ArithmeticMyException("Overflow exception min: " + expression + "");
        return -expression;
    }

    @Override
    public double consider(double expression) {
        return -expression;
    }

    public String getOperator() {
        return "-";
    }

}
