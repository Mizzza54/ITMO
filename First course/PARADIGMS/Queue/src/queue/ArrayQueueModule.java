package queue;


/**
 * @author Michale Gerasimov
 * start: 24.02.2020
 * @version -
 */

// INV:
// ((size == 0 && start == end) || (size > 0 && start != end)) && size < elements.length
public class ArrayQueueModule {
    private static int size;
    private static int start;
    private static int end;
    private static Object[] elements = new Object[5];

    // Pre: element ≠ null
    // Post: n = n' + 1 ∧ ∀i=head..n' : a[i]' = a[i] ∧ a[n] = element
    public static void enqueue(Object element) {
        assert element != null;

        ensureCapacity(size + 1);
        elements[end] = element;
        end = getNextPos(end);
        size++;
    }

    // Pre: n > 0
    // Post: ℝ = a[head] ∧ n = n' ∧ ∀i=head..n : a[i]' = a[i]
    public static Object element() {
        assert size > 0;

        return elements[start];
    }

    // Pre: n > 0
    // Post: ℝ = a[head] ∧ n = n' − 1 ∧ ∀i=head..n : a[i]' = a[i]
    public static Object dequeue() {
        assert size > 0;

        Object value = element();
        size--;
        start = getNextPos(start);
        return value;
    }

    // Post: ℝ = n ∧ n = n' ∧ ∀i=head..n : a[i]' = a[i]
    public static int size() {
        return size;
    }

    // Post: ℝ = n > 0 ∧ n = n' ∧ ∀i=head..n : a[i]' = a[i]
    public static boolean isEmpty() {
        return size == 0;
    }

    // Post: n = 0
    public static void clear() {
        start = 0;
        end = start;
        size = 0;
    }

    // Post: R = StringOf(elements) ∧ n = n' ∧ ∀i=head..n : a[i]' = a[i]
    public static String toStr() {
        if (size == 0) {
            return "[]";
        }

        StringBuilder sb = new StringBuilder();
        int i = getNextPos(start);
        sb.append("[" + element().toString());
        while (i != start && i != end) {
            sb.append(", " + elements[i].toString());
            i = getNextPos(i);
        }
        sb.append("]");
        return sb.toString();
    }

    // Pre: 0 < capacity < n
    // Post: ( (∀i=head..n : a[i]' = a[i]) ∨
    //       (for all i = start, j = 0; i != end; i = getNextPos(i), j++: elements'[j] == elements[i])) ∧
    //       (n = n' ∨ n = n' * 2)
    private static void ensureCapacity(int capacity) {
        if (capacity < elements.length) {
            return;
        }
        int newCapacity = 2 * capacity;
        Object[] newElements = new Object[newCapacity];
        copy(elements, newElements, start, end, size);
        elements = newElements;
        start = 0;
        end = start + size;
    }

    // Pre: 0 <= pos < n
    // Post: R = ((pos + 1) % elements.length) ∧
    //       ∀i=head..n : a[i]' = a[i]
    private static int getNextPos(int pos) {
        return (pos + 1) % elements.length;
    }

    private static void copy(Object[] src, Object[] dest, int start, int end, int size) {
        if (start > end) {
            System.arraycopy(src, start, dest, 0, src.length - start);
            System.arraycopy(src, 0, dest, src.length - start, end);
        } else {
            System.arraycopy(src, start, dest, 0, size);
        }
    }
}