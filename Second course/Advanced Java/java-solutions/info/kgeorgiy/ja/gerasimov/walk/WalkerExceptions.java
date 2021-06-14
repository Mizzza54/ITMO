package info.kgeorgiy.ja.gerasimov.walk;

/**
 * @author Michael Gerasimov
 * start: 13.02.2021
 * @version -
 */
public class WalkerExceptions extends Exception {
    public WalkerExceptions(String message, Exception e) {
        super(e.getMessage() + "\nError: " + message);
    }

    public WalkerExceptions(String message) {
        super("\nError: " + message);
    }
}
