package ru.ifmo.gerasimov.core.exceptions;

/**
 * @author Michael Gerasimov
 */
public class WrappedException extends Exception {

    public WrappedException() {
        super();
    }

    public WrappedException(String message) {
        super(message);
    }

    public WrappedException(String message, Throwable cause) {
        super(message, cause);
    }

    public WrappedException(Throwable cause) {
        super(cause);
    }

    public WrappedException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
