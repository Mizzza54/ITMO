package expression.generic.numbers;

import expression.generic.MyNumber;

/**
 * @author Michale Gerasimov
 * start: 17.03.2020
 * @version -
 */
public class LongNumbers extends MyNumber<Long> {
    @Override
    public Long add(Long a, Long b) {
        return a + b;
    }

    @Override
    public Long subtract(Long a, Long b) {
        return a - b;
    }

    @Override
    public Long divide(Long a, Long b) {
        return a / b;
    }

    @Override
    public Long multiply(Long a, Long b) {
        return a * b;
    }

    @Override
    public Long negate(Long a) {
        return -a;
    }

    @Override
    public Long parseToDigit(String str) {
        return Long.parseUnsignedLong(str);
    }

    @Override
    public Long parseToDigitTab(String str) {
        return Long.parseLong(str);
    }
}
