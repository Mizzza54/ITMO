package queue;

import java.util.Random;

/**
 * @author Michale Gerasimov
 * start: 25.02.2020
 * @version -
 */
public class Main {
    public static Random random = new Random();


    public static void main(String[] args) {

        LinkedQueue queue = new LinkedQueue();
        queue.enqueue(1);
        queue.enqueue(2);
        queue.enqueue(3);
        Object[] obj = queue.toArray();
        for (int i = 0; i < obj.length; i++) {
            System.out.println(obj[i]);
        }


        /*
        System.out.println("----------------------------------ArrayQueueModuleTesting----------------------------------");
        ArrayQueueModuleTesting();
        System.out.println();
        System.out.println("----------------------------------ArrayQueueADTTesting-------------------------------------");
        ArrayQueueADTTesting();
        System.out.println();
        System.out.println("----------------------------------ArrayQueueTesting----------------------------------------");
        ArrayQueueTesting();
         */
    }

    public static void ArrayQueueModuleTesting() {
        for (int i = 1; i < 10; i++) {
            ArrayQueueModule.enqueue(i);
        }
        System.out.println("size = " + ArrayQueueModule.size());
        for (int i = 1; i < 10; i++) {
            System.out.print(ArrayQueueModule.dequeue() + " ");
        }
        System.out.println();
        System.out.println("Empty = " + ArrayQueueModule.isEmpty());
        for (int i = 1; i < 10; i++) {
            ArrayQueueModule.enqueue(random.nextInt());
        }
        System.out.println("Empty = " + ArrayQueueModule.isEmpty());
        System.out.println(ArrayQueueModule.element());
        System.out.println(ArrayQueueModule.toStr());
        ArrayQueueModule.clear();
        System.out.println("Empty = " + ArrayQueueModule.isEmpty());
    }

    public static void ArrayQueueADTTesting() {
        ArrayQueueADT queueADT = new ArrayQueueADT();
        for (int i = 1; i < 10; i++) {
            ArrayQueueADT.enqueue(queueADT, i);
        }
        System.out.println("size = " + ArrayQueueADT.size(queueADT));
        for (int i = 1; i < 10; i++) {
            System.out.print(ArrayQueueADT.dequeue(queueADT) + " ");
        }
        System.out.println();
        System.out.println("Empty = " + ArrayQueueADT.isEmpty(queueADT));
        for (int i = 1; i < 10; i++) {
            ArrayQueueADT.enqueue(queueADT, random.nextInt());
        }
        System.out.println("Empty = " + ArrayQueueADT.isEmpty(queueADT));
        System.out.println(ArrayQueueADT.element(queueADT));
        System.out.println(ArrayQueueADT.toStr(queueADT));
        ArrayQueueADT.clear(queueADT);
        System.out.println("Empty = " + ArrayQueueADT.isEmpty(queueADT));
    }

    public static void ArrayQueueTesting() {
        ArrayQueue queue = new ArrayQueue();
        for (int i = 1; i < 10; i++) {
            queue.enqueue(i);
        }
        System.out.println("size = " + queue.size());
        for (int i = 1; i < 10; i++) {
            System.out.print(queue.dequeue() + " ");
        }
        System.out.println();
        System.out.println("Empty = " + queue.isEmpty());
        for (int i = 1; i < 10; i++) {
            queue.enqueue(random.nextInt());
        }
        System.out.println("Empty = " + queue.isEmpty());
        System.out.println(queue.element());
        System.out.println(queue.toStr());
        queue.clear();
        System.out.println("Empty = " + queue.isEmpty());
    }
}
