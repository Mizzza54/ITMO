package queue;


/**
 * @author Michale Gerasimov
 * start: 24.02.2020
 * @version -
 */

// INV:
// ((size == 0 && start == end) || (size > 0 && start != end)) && size < elements.length
public class ArrayQueue extends AbstractQueue {
    private int size;
    private int start;
    private int end;
    private Object[] elements = new Object[5];

    // Pre: element ≠ null
    // Post: n = n' + 1 ∧ ∀i=head..n' : a[i]' = a[i] ∧ a[n] = element
    @Override
    protected void enqueueImpl(Object element) {
        ensureCapacity(size() + 1);
        elements[end] = element;
        end = getNextPos(end);
    }

    // Pre: n > 0
    // Post: ℝ = a[head] ∧ n = n' ∧ ∀i=head..n : a[i]' = a[i]
    @Override
    protected Object elementImpl() {
        return elements[start];
    }

    // Pre: n > 0
    // Post: ℝ = a[head] ∧ n = n' − 1 ∧ ∀i=head..n : a[i]' = a[i]
    @Override
    protected void dequeueImpl() {
        start = getNextPos(start);
    }

    // Post: ℝ = n > 0 ∧ n = n' ∧ ∀i=head..n : a[i]' = a[i]
    public boolean isEmpty() {
        return size() == 0;
    }

    // Post: n = 0
    @Override
    protected void clearImpl() {
        start = 0;
        end = start;
    }

    // Post: R = StringOf(elements) ∧ n = n' ∧ ∀i=head..n : a[i]' = a[i]
    public String toStr() {
        if (size() == 0) {
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

    private void ensureCapacity(int capacity) {
        if (capacity < elements.length) {
            return;
        }
        int newCapacity = 2 * capacity;
        Object[] newElements = new Object[newCapacity];
        copy(elements, newElements, start, end, size());
        elements = newElements;
        start = 0;
        end = start + size();
    }


    private int getNextPos(int pos) {
        return (pos + 1) % elements.length;
    }

    private void copy(Object[] src, Object[] dest, int start, int end, int size) {
        if (start > end) {
            System.arraycopy(src, start, dest, 0, src.length - start);
            System.arraycopy(src, 0, dest, src.length - start, end);
        } else {
            System.arraycopy(src, start, dest, 0, size());
        }
    }
}