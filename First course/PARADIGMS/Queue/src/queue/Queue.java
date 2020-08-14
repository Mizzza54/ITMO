package queue;

import java.util.function.Predicate;
import java.util.function.Function;

/**
 * @author Michale Gerasimov
 * start: 03.03.2020
 * @version -
 */

public interface Queue {

    // Post: ℝ = n ∧ n = n' ∧ ∀i=head..n : a[i]' = a[i]
    int size();

    // Pre: element ≠ null
    // Post: n = n' + 1 ∧ ∀i=head..n' : a[i]' = a[i] ∧ a[n] = element
    void enqueue(Object elem);

    // Pre: n > 0
    // Post: ℝ = a[head] ∧ n = n' ∧ ∀i=head..n : a[i]' = a[i]
    Object element();

    // Pre: n > 0
    // Post: ℝ = a[head] ∧ n = n' − 1 ∧ ∀i=head..n : a[i]' = a[i]
    Object dequeue();

    // Post: ℝ = n > 0 ∧ n = n' ∧ ∀i=head..n : a[i]' = a[i]
    boolean isEmpty();

    // Post: n = 0
    void clear();

    // Post: R = ArrayOf(elements) ∧ n = n' ∧ ∀i=head..n : a[i]' = a[i]
    Object[] toArray();
}