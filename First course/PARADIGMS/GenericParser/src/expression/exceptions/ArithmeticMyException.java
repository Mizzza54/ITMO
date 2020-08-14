package expression.exceptions;

/**
 * @author Michale Gerasimov
 * start: 05.02.2020
 * @version -
 */
public class ArithmeticMyException extends ArithmeticException {
    public ArithmeticMyException(String message) {
        super(message + "\n");
    }
}
