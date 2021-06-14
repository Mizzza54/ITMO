package info.kgeorgiy.ja.gerasimov.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author Michael Gerasimov
 * start: 16.04.2021
 * @version -
 */
public class ParallelMapperImpl implements ParallelMapper {
    private final Queue<Runnable> tasks;
    private final List<Thread> workers;

    public ParallelMapperImpl(final int threads) throws InterruptedException {
        InterruptedException exception = new InterruptedException();
        tasks = new ArrayDeque<>();
        final Runnable worker = () -> {
            try {
                while (!Thread.interrupted()) {
                    Runnable task;
                    synchronized (tasks) {
                        while (tasks.isEmpty()) {
                            tasks.wait();
                        }
                        task = tasks.poll();
                        tasks.notify();
                    }
                    task.run();
                }
            } catch (InterruptedException e) {
                exception.addSuppressed(e);
            } finally {
                Thread.currentThread().interrupt();
            }
        };
        workers = Stream.generate(() -> new Thread(worker)).limit(threads).collect(Collectors.toList());
        workers.forEach(Thread::start);
        // :NOTE: does not work as intended in a vast majority of cases
        if (exception.getSuppressed().length != 0) {
            throw exception;
        }
    }


    private static class ConcurrentList<T> {
        private final List<T> array;
        private volatile long countReadyTasks;

        private ConcurrentList(final int size) {
            this.array = new ArrayList<>(Collections.nCopies(size, null));
            countReadyTasks = 0;
        }

        public synchronized void set(final int index, final T value) {
            array.set(index, value);
            countReadyTasks++;
            if (countReadyTasks == getSize()) {
                notify();
            }
        }

        public synchronized List<T> getResult() throws InterruptedException {
            while (countReadyTasks < getSize()) {
                wait();
            }
            return array;
        }

        synchronized long getSize() {
            return array.size();
        }
    }

    @Override
    public <T, R> List<R> map(final Function<? super T, ? extends R> function,
                              final List<? extends T> values) throws InterruptedException {
        ConcurrentList<R> concurrentList = new ConcurrentList<>(values.size());
        // :NOTE: too wide of a synchronized
        synchronized (tasks) {
            for (int i = 0; i < values.size(); i++) {
                final int taskIndex = i;
                Runnable task = () -> concurrentList.set(taskIndex, function.apply(values.get(taskIndex)));
                tasks.add(task);
                tasks.notify();
            }
        }
        return concurrentList.getResult();
    }

    @Override
    public void close() {
        workers.forEach(Thread::interrupt);
        InterruptedException exception = new InterruptedException();
        workers.forEach(thread -> {
            try {
                thread.join();
            } catch (InterruptedException e) {
                exception.addSuppressed(e);
            }
        });

        if (exception.getSuppressed().length != 0) {
            exception.printStackTrace();
        }
    }
}