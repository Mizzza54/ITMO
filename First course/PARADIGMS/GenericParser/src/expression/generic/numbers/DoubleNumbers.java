package expression.generic.numbers;

import expression.generic.MyNumber;

/**
 * @author Michale Gerasimov
 * start: 10.03.2020
 * @version -
 */
public class DoubleNumbers extends MyNumber<Double> {

    @Override
    public Double add(Double a, Double b) {
        return a + b;
    }

    @Override
    public Double subtract(Double a, Double b) {
        return a - b;
    }

    @Override
    public Double divide(Double a, Double b) {
        return a / b;
    }

    @Override
    public Double multiply(Double a, Double b) {
        return a * b;
    }

    @Override
    public Double negate(Double a) {
        return -a;
    }

    @Override
    public Double parseToDigit(String str) {
        return Double.parseDouble(str);
    }

    @Override
    public Double parseToDigitTab(String str) {
        return Double.parseDouble(str);
    }
}