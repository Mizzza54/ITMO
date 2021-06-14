package info.kgeorgiy.ja.gerasimov.concurrent;
import info.kgeorgiy.java.advanced.concurrent.AdvancedIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;
import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * @author Michael Gerasimov
 * start: 16.04.2021
 * @version -
 */
public class IterativeParallelism implements AdvancedIP {

    private final ParallelMapper mapper;

    public IterativeParallelism(ParallelMapper mapper) {
        this.mapper = mapper;
    }

    public IterativeParallelism() {
        this(null);
    }

    private <T> List<Stream<T>> partition(int threads, List<T> values) {
        int count = values.size() / threads;
        int[] partitionArray = new int[threads];
        int temp = values.size() % threads;
        partitionArray[0] = temp == 0 ? count : count + 1;
        for (int i = 1; i < threads; i++) {
            if (i < temp) {
                partitionArray[i] = partitionArray[i - 1] + count + 1;
            } else {
                partitionArray[i] = partitionArray[i - 1] + count;
            }
        }
        // :NOTE: unfair partition

        List<Stream<T>> tasksPartition = new ArrayList<>(Collections.nCopies(threads, null));
        tasksPartition.set(0, values.subList(0, partitionArray[0]).stream());
        for (int i = 1; i < threads; i++) {
            tasksPartition.set(i, values.subList(partitionArray[i - 1], partitionArray[i]).stream());
        }
        return tasksPartition;
    }

    private<T, R> R addTask(int threads,
                            List<T> values,
                            Function<Stream<T>, R> function,
                            Function<Stream<R>, R> collector) throws InterruptedException {
        if (threads <= 0) {
            throw new NoSuchElementException();
        }

        threads = Math.min(threads, values.size());
        List<Stream<T>> tasksPartition = partition(threads, values);
        List<R> result;

        if (mapper != null) {
            result = mapper.map(function, tasksPartition);
        } else {
            InterruptedException exception = null;
            List<Worker<T, R>> workers = IntStream.range(0, threads)
                    .mapToObj(i -> new Worker<>(tasksPartition.get(i), function))
                    .collect(Collectors.toList());

            for (Worker<T, R> worker: workers) {
                try {
                    worker.getThread().join();
                } catch (InterruptedException e) {
                    if (exception == null) {
                        exception = e;
                    } else {
                        exception.addSuppressed(e);
                    }
                }
            }

            if (exception != null) {
                throw exception;
            }

            result = workers.stream().map(Worker::getResult).collect(Collectors.toList());
        }

        return collector.apply(result.stream());
    }

    @Override
    public <T> T maximum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        return addTask(threads, values, s -> s.max(comparator).orElseThrow(), s -> s.max(comparator).orElseThrow());
    }

    @Override
    public <T> T minimum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        return maximum(threads, values, comparator.reversed());
    }

    @Override
    public <T> boolean all(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return !any(threads, values, predicate.negate());
    }

    @Override
    public <T> boolean any(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return addTask(threads, values, s -> s.anyMatch(predicate), (list) -> list.anyMatch(item -> item));
    }

    @Override
    public String join(int threads, List<?> values) throws InterruptedException {
        return addTask(threads, values, s -> s.map(Object::toString).collect(Collectors.joining()), r -> r.collect(Collectors.joining()));
    }

    @Override
    public <T> List<T> filter(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        // :NOTE: intermediate lists
        return addTask(threads, values, s -> s.filter(predicate).collect(Collectors.toList()),
                r -> r.flatMap(Collection::stream).collect(Collectors.toList()));
    }

    @Override
    public <T, U> List<U> map(int threads, List<? extends T> values, Function<? super T, ? extends U> f) throws InterruptedException {
        return addTask(threads, values, s -> s.map(f).collect(Collectors.toList()), r -> r.flatMap(Collection::stream).collect(Collectors.toList()));
    }

    @Override
    public <T> T reduce(int threads, List<T> values, AdvancedIP.Monoid<T> monoid) throws InterruptedException {
        return addTask(threads, values, s -> s.reduce(monoid.getIdentity(), monoid.getOperator()), r -> r.reduce(monoid.getIdentity(), monoid.getOperator()));
    }

    @Override
    public <T, R> R mapReduce(int threads, List<T> values, Function<T, R> lift, AdvancedIP.Monoid<R> monoid) throws InterruptedException {
        return  addTask(threads, values, s -> s.map(lift).reduce(monoid.getIdentity(), monoid.getOperator()), r -> r.reduce(monoid.getIdentity(), monoid.getOperator()));
    }
}
