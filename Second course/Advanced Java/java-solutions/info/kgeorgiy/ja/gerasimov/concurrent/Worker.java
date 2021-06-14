package info.kgeorgiy.ja.gerasimov.concurrent;

import java.util.function.Function;
import java.util.stream.Stream;

/**
 * @author Michael Gerasimov
 * start: 11.04.2021
 * @version -
 */
public class Worker<T, R> implements Runnable {
    private final Thread thread;
    private final Stream<T> array;
    private final Function<Stream<T>, R> function;
    private R result;

    public Worker(Stream<T> subList, Function<Stream<T>, R> function) {
        this.array = subList;
        this.function = function;
        thread = new Thread(this);
        thread.start();
    }

    @Override
    public void run() {
        result = function.apply(array);
    }

    public Thread getThread() {
        return this.thread;
    }

    public R getResult() {
        return this.result;
    }
}
