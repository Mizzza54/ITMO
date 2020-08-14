package operation.check;

import expression.CommonExpression;
import expression.exceptions.ArithmeticMyException;
import operation.UnaryOperation;


/**
 * @author Michale Gerasimov
 * start: 11.02.2020
 * @version -
 */
public class CheckedLog2 extends UnaryOperation {
    public CheckedLog2(CommonExpression expression) {
        super(expression);
    }

    @Override
    public int consider(int expression) throws ArithmeticMyException {
        if (expression <= 0) {
            throw new ArithmeticMyException("Argument of log is negative: " + expression );
        }
        int res = 0;
        while (expression != 1) {
            expression = expression / 2;
            res++;
        }
        return res;
    }

    @Override
    public double consider(double expression) {
        return (Math.log(expression)/Math.log(2));
    }

    public String getOperator() {
        return "log2";
    }
}
