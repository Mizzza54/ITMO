package expression.generic.numbers;

import expression.exceptions.ArithmeticMyException;
import expression.generic.MyNumber;

/**
 * @author Michale Gerasimov
 * start: 10.03.2020
 * @version -
 */
public class IntegerNumbers extends MyNumber<Integer> {

    @Override
    public Integer add(Integer left, Integer right) {
        if (right > 0) {
            if (left > Integer.MAX_VALUE - right) {
                throw new ArithmeticMyException("Overflow exception : " + left + " + " + right);
            }
        } else {
            if (left < Integer.MIN_VALUE - right) {
                throw new ArithmeticMyException("Overflow exception : " + left + " + " + right);
            }
        }
        return left + right;
    }

    @Override
    public Integer subtract(Integer left, Integer right) {
        if (right > 0) {
            if (left < Integer.MIN_VALUE + right) {
                throw new ArithmeticMyException("Overflow exception : " + left + " - " + right);
            }
        } else {
            if (left > Integer.MAX_VALUE + right) {
                throw new ArithmeticMyException("Overflow exception : " + left + " - " + right);
            }
        }
        return left - right;
    }

    @Override
    public Integer divide(Integer left, Integer right) {
        if (right == 0) {
            throw new ArithmeticMyException("Division by zero : " + left + " / " + right);
        }
        if (left == Integer.MIN_VALUE && right == -1) {
            throw new ArithmeticMyException("Overflow exception : " + left + " / " + right);
        }
        return left / right;
    }

    @Override
    public Integer multiply(Integer left, Integer right) {
        if (left > 0 && right > 0 && Integer.MAX_VALUE / left < right) {
            throw new ArithmeticMyException("Overflow exception : " + left + " * " + right);
        }
        if (left > 0 && right < 0 && Integer.MIN_VALUE / left > right) {
            throw new ArithmeticMyException("Overflow exception : " + left + " * " + right);
        }
        if (left < 0 && right > 0 && Integer.MIN_VALUE / right > left) {
            throw new ArithmeticMyException("Overflow exception : " + left + " * " + right);
        }
        if (left < 0 && right < 0 && Integer.MAX_VALUE / left > right) {
            throw new ArithmeticMyException("Overflow exception : " + left + " * " + right);
        }
        return left * right;
    }

    @Override
    public Integer negate(Integer a) {
        if (a < Integer.MIN_VALUE)
            throw new ArithmeticMyException("Overflow exception min: " + a + "");
        return -a;
    }

    @Override
    public Integer parseToDigit(String str) {
        return Integer.parseUnsignedInt(str);
    }

    @Override
    public Integer parseToDigitTab(String str) {
        return Integer.parseInt(str);
    }
}
