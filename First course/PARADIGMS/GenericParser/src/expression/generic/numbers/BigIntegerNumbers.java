package expression.generic.numbers;

import expression.exceptions.ArithmeticMyException;
import expression.generic.MyNumber;

import java.math.BigInteger;

/**
 * @author Michale Gerasimov
 * start: 10.03.2020
 * @version -
 */
public class BigIntegerNumbers extends MyNumber<BigInteger> {

    @Override
    public BigInteger add(BigInteger a, BigInteger b) {
        return a.add(b);
    }

    @Override
    public BigInteger subtract(BigInteger a, BigInteger b) {
        return a.subtract(b);
    }

    @Override
    public BigInteger divide(BigInteger a, BigInteger b) {
        return a.divide(b);
    }

    @Override
    public BigInteger multiply(BigInteger a, BigInteger b) {
        return a.multiply(b);
    }

    @Override
    public BigInteger negate(BigInteger a) {
        return a.negate();
    }

    @Override
    public BigInteger parseToDigit(String str) {
        return new BigInteger(str);
    }

    @Override
    public BigInteger parseToDigitTab(String str) {
        return new BigInteger(str);
    }
}
