package operation;

import expression.CommonExpression;

/**
 * @author Michale Gerasimov
 * start: 10.12.2019
 * @version 10.12.2019
 */
public class Negative extends UnaryOperation {

    public Negative(CommonExpression expression) {
        super(expression);
    }

    @Override
    public int consider(int expression) {
        return -expression;
    }

    @Override
    public double consider(double expression) {
        return -expression;
    }

    @Override
    public String getOperator() {
        return "-";
    }
}
