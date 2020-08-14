package operation.check;

import expression.CommonExpression;
import expression.exceptions.ArithmeticMyException;
import operation.UnaryOperation;

/**
 * @author Michale Gerasimov
 * start: 12.02.2020
 * @version -
 */
public class CheckedPow2 extends UnaryOperation {
    public CheckedPow2(CommonExpression expression) {
        super(expression);
    }

    @Override
    public int consider(int expression) throws ArithmeticMyException {
        if (expression < 0 ) {
            throw new ArithmeticMyException("NOT NEGATIVE POW");
        }
        if (expression > 30) {
            throw new ArithmeticMyException("Overflow exception OF POW2");
        }
        int result = 1;
        for (int i = 0; i < expression; i++) {
            result *= 2;
        }
        return result;
    }

    @Override
    public double consider(double expression) {
        return 0;
    }

    public String getOperator() {
        return null;
    }
}
