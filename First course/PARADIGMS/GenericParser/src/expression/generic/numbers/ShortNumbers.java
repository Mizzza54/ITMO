package expression.generic.numbers;

import expression.generic.MyNumber;

/**
 * @author Michale Gerasimov
 * start: 17.03.2020
 * @version -
 */
public class ShortNumbers extends MyNumber<Short> {
    @Override
    public Short add(Short a, Short b) {
        return (short) (a + b);
    }

    @Override
    public Short subtract(Short a, Short b) {
        return (short) (a - b);
    }

    @Override
    public Short divide(Short a, Short b) {
        return (short) (a / b);
    }

    @Override
    public Short multiply(Short a, Short b) {
        return (short) (a * b);
    }

    @Override
    public Short negate(Short a) {
        return (short) (-a);
    }

    @Override
    public Short parseToDigit(String str) {
        return (short) Integer.parseUnsignedInt(str);
    }

    @Override
    public Short parseToDigitTab(String str) {
        return (short) Integer.parseInt(str);
    }
}
