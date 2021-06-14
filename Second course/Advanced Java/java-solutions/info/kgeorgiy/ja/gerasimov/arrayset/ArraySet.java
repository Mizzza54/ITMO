package info.kgeorgiy.ja.gerasimov.arrayset;

import java.util.*;

/**
 * @author Michael Gerasimov
 * start: 21.02.2021
 * @version -
 */
public class ArraySet<T> extends AbstractSet<T> implements NavigableSet<T> {

    public static void main(String[] args) {

    }

    private final List<T> List;
    private final Comparator<? super T> Comparator;

    public ArraySet() {
        this(null, null);
    }

    public ArraySet(Collection<? extends T> Collection) {
        this(Collection, null);
    }

    public ArraySet(Comparator<? super T> Comparator) {
        this (null, Comparator);
    }

    public ArraySet(Collection<? extends T> Collection, Comparator<? super T> Comparator) {
        if (Collection == null) {
            this.List = new ArrayList<>();
        } else if (isSorted(Collection, Comparator)) {
            this.List = new ArrayList<>(Collection);
        } else {
            TreeSet<T> Temp = new TreeSet<>(Comparator);
            Temp.addAll(Collection);
            this.List = new ArrayList<>(Temp);
        }

        this.Comparator = Comparator;
    }

    private boolean isSorted(Collection<? extends T> collection, Comparator<? super T> Comparator) {
        if (collection == null || collection.isEmpty()) {
            return true;
        }

        Iterator<? extends T> iterator = collection.iterator();
        T prev = iterator.next();
        T cur;
        while (iterator.hasNext()) {
            cur = iterator.next();
            if (compare(cur, prev, Comparator) <= 0) {
                return false;
            }
            prev = cur;
        }
        return true;
    }

    @SuppressWarnings("unchecked")
    private int compare(T o1, T o2, Comparator<? super T> Comparator) {
        if (Comparator == null && o1 instanceof Comparable) {
            return ((Comparable<T>) o1).compareTo(o2);
        } else {
            if (Comparator == null) {
                throw new ClassCastException("Comparator is null and Elements don't comparable");
            }
            return Comparator.compare(o1, o2);
        }
    }

    /**
     * if key not found
     * binarySearch return -(low + 1)
     */
    public int search(T elem) {
        if (elem == null) {
            throw new NullPointerException("search(T elem) -> elem is null");
        }

        int index = Collections.binarySearch(List, elem, Comparator);
        if (index < 0) {
            index *= -1;
            index -= 1;
        }
        return index < List.size() ? index : List.size() - 1;
    }


    /**
     * sign == true -> ">"
     * sign == false -> "<"
     */
    private T customSearch(T elem, boolean sign, boolean inclusive, int difference) {
        if (List.isEmpty()) {
            return null;
        }

        int index = search(elem);
        int resultOfCompare = compare(List.get(index), elem, Comparator);
        resultOfCompare = sign ? resultOfCompare : (-1) * resultOfCompare;

        if (inclusive) {
            if (resultOfCompare >= 0) {
                index += difference;
            }
        } else {
            if (resultOfCompare > 0) {
                index += difference;
            }
        }

        return (index >= 0 && index <= List.size() - 1 ? List.get(index) : null);
    }

    /**
     * lower() – возвращает наибольший элемент в наборе,
     * но строго меньше чём заданный если такого элемента нет,
     * то в результате будет возвращено null.
     */
    @Override
    public T lower(T t) {
        return customSearch(t, true, true, -1);
    }

    /**
     * floor()– возвращает наибольший элемент в наборе,
     * но меньше чём заданный или равный ему,
     * в случае отсутствия такого элемента будет возвращено null.
     */
    @Override
    public T floor(T t) {
        return customSearch(t, true, false, -1);
    }

    /**
     * ceiling() – возвращает ближайший элемент в наборе,
     * но который больше или равняется заданному,
     * в случае отсутствия такого элемента будет возвращено null.
     */
    @Override
    public T ceiling(T t) {
        return customSearch(t, false, false, +1);
    }

    /**
     * higher() – возвращает ближайший элемент в наборе,
     * но строго больше чём заданный,
     * в случае отсутствия такого элемента будет возвращено null.
     */
    @Override
    public T higher(T t) {
        return customSearch(t, false, true, +1);
    }

    @Override
    public T pollFirst() {
        throw new UnsupportedOperationException("Unsupported operation: pollFirst");
    }

    @Override
    public T pollLast() {
        throw new UnsupportedOperationException("Unsupported operation: pollLast");
    }

    @Override
    public Iterator<T> iterator() {
        return Collections.unmodifiableList(List).iterator();
    }

    @Override
    public NavigableSet<T> descendingSet() {
        return new ArraySet<>(List, Collections.reverseOrder(Comparator));
    }

    @Override
    public Iterator<T> descendingIterator() {
        return descendingSet().iterator();
    }

    @Override
    public NavigableSet<T> subSet(T fromElement, boolean fromInconclusive, T toElement, boolean toInconclusive) {
        if (compare(fromElement, toElement, Comparator) > 0) {
            throw new IllegalArgumentException("Comparator = " + Comparator.toString() + " -> subSet(T fromElement, T toElement) -> Bad arguments: fromElement(" + fromElement + ") > toElement(" + toElement + ")");
        }

        int fromIndex = search(fromElement);
        int toIndex = search(toElement);

        if (fromIndex > toIndex || fromIndex == -1 || toIndex == -1) {
            return new ArraySet<>(Comparator);
        }

        fromIndex = !fromInconclusive && compare(fromElement, List.get(fromIndex), Comparator) == 0 ? fromIndex + 1 : fromIndex;
        toIndex = toInconclusive && compare(toElement, List.get(toIndex), Comparator) == 0 ? toIndex + 1 : toIndex;

        fromIndex = fromIndex == size() - 1 && compare(fromElement, List.get(fromIndex), Comparator) > 0 ? fromIndex + 1 : fromIndex;
        toIndex = toIndex != size() && compare(toElement, List.get(toIndex), Comparator) > 0 ? toIndex + 1 : toIndex;

        if (fromIndex > toIndex) {
            return new ArraySet<>(Comparator);
        }

        return new ArraySet<>(List.subList(fromIndex, toIndex), Comparator);
    }

    @Override
    public NavigableSet<T> headSet(T toElement, boolean inclusive) {
        if (List.isEmpty()) {
            return this;
        }

        if (compare(first(), toElement, Comparator) > 0) {
            return new ArraySet<>(Comparator);
        }

        return subSet(first(), true, toElement, inclusive);
    }

    @Override
    public NavigableSet<T> tailSet(T fromElement, boolean inclusive) {
        if (List.isEmpty()) {
            return this;
        }

        if (compare(fromElement, last(), Comparator) > 0) {
            return new ArraySet<>(Comparator);
        }

        return subSet(fromElement, inclusive, last(), true);
    }

    @Override
    public Comparator<? super T> comparator() {
        return Comparator;
    }

    @Override
    public SortedSet<T> subSet(T fromElement, T toElement) throws IllegalArgumentException {
        return subSet(fromElement, true, toElement, false);
    }

    @Override
    public SortedSet<T> headSet(T toElement) {
        return headSet(toElement, false);
    }

    @Override
    public SortedSet<T> tailSet(T fromElement) {
        return tailSet(fromElement, true);
    }

    @Override
    public T first() {
        if (List.isEmpty()) {
            throw new NoSuchElementException("first() -> Empty ArraySet");
        }
        return List.get(0);
    }

    @Override
    public T last() {
        if (List.isEmpty()) {
            throw new NoSuchElementException("last() -> Empty ArraySet");
        }
        return List.get(size() - 1);
    }

    @Override
    public int size() {
        return List.size();
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean contains(Object o) {
        if (o == null) {
            throw new NullPointerException("contains(Object o) -> o is null");
        }
        return Collections.binarySearch(List, (T) o, Comparator) >= 0;
    }
}
