package queue;


/**
 * @author Michale Gerasimov
 * start: 24.02.2020
 * @version -
 */

// INV:
// ((q.size == 0 && q.start == q.end) || (q.size > 0 && q.start != q.end)) && q.size < q.elements.length
public class ArrayQueueADT {
    private int size;
    private int start;
    private int end;
    private Object[] elements = new Object[5];

    // Pre: element ≠ null
    // Post: q.n = q.n' + 1 ∧ ∀i=q.head..q.n' : q.a[i]' = q.a[i] ∧ q.a[n] = element
    public static void enqueue(ArrayQueueADT q, Object element) {
        assert element != null;

        ensureCapacity(q, q.size + 1);
        q.elements[q.end] = element;
        q.end = getNextPos(q, q.end);
        q.size++;
    }

    // Pre: q.n > 0
    // Post: ℝ = q.a[q.head] ∧ q.n = q.n' ∧ ∀i=q.head..q.n : q.a[i]' = q.a[i]
    public static Object element(ArrayQueueADT q) {
        assert q.size > 0;

        return q.elements[q.start];
    }

    // Pre: q.n > 0
    // Post: ℝ = q.a[q.head] ∧ q.n = q.n' − 1 ∧ ∀i=q.head..q.n : q.a[i]' = q.a[i]
    public static Object dequeue(ArrayQueueADT q) {
        assert q.size > 0;

        Object value = element(q);
        q.size--;
        q.start = getNextPos(q, q.start);
        return value;
    }

    // Post: ℝ = q.n ∧ q.n = q.n' ∧ ∀i=q.head..q.n : q.a[i]' = q.a[i]
    public static int size(ArrayQueueADT q) {
        return q.size;
    }

    // Post: ℝ = q.n > 0 ∧ q.n = q.n' ∧ ∀i=q.head..n : q.a[i]' = q.a[i]
    public static boolean isEmpty(ArrayQueueADT q) {
        return q.size == 0;
    }

    // Post: q.n = 0
    public static void clear(ArrayQueueADT q) {
        q.start = 0;
        q.end = q.start;
        q.size = 0;
    }

    // Post: R = StringOf(q.elements) ∧ q.n = q.n' ∧ ∀i=q.head..q.n : q.a[i]' = q.a[i]
    public static String toStr(ArrayQueueADT q) {
        if (q.size == 0) {
            return "[]";
        }


        StringBuilder sb = new StringBuilder();
        int i = getNextPos(q, q.start);
        sb.append("[" + element(q).toString());
        while (i != q.start && i != q.end) {
            sb.append(", " + q.elements[i].toString());
            i = getNextPos(q, i);
        }
        sb.append("]");
        return sb.toString();
    }

    // Pre: 0 < capacity < q.n
    // Post: ( (∀i=q.head..q.n : q.a[i]' = q.a[i]) ∨
    //       (for all i = q.head, j = 0; i != q.tail; i = getNextPos(i), j++: q.elements'[j] == q.elements[i])) ∧
    //       (q.n = q.n' ∨ q.n = q.n' * 2)
    private static void ensureCapacity(ArrayQueueADT q, int capacity) {
        if (capacity < q.elements.length) {
            return;
        }
        int newCapacity = 2 * capacity;
        Object[] newElements = new Object[newCapacity];
        copy(q.elements, newElements, q.start, q.end, q.size);
        q.elements = newElements;
        q.start = 0;
        q.end = q.start + q.size;
    }

    // Pre: 0 <= pos < q.n
    // Post: R = ((pos + 1) % q.elements.length) ∧
    //       ∀i=q.head..q.n : q.a[i]' = q.a[i]
    private static int getNextPos(ArrayQueueADT q, int pos) {
        return (pos + 1) % q.elements.length;
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
