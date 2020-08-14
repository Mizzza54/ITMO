package operation;

import expression.CommonExpression;

/**
 * @author Michale Gerasimov
 * start: 10.12.2019
 * @version 10.12.2019
 */
public abstract class UnaryOperation implements CommonExpression {
    private final CommonExpression expression;

    public UnaryOperation(CommonExpression expression) {
        this.expression = expression;
    }

    abstract public int consider(int expression);

    abstract public double consider(double expression);

    @Override
    public double evaluate(double x) {
        return consider(expression.evaluate(x));
    }

    @Override
    public int evaluate(int x) {
        return consider(expression.evaluate(x));
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return consider(expression.evaluate(x, y, z));
    }

    public String toString() {
        return "(" + getOperator() + expression + ")";
    }

    public abstract String getOperator();
}
