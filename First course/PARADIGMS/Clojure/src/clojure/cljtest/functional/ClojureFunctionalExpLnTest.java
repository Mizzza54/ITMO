package clojure.cljtest.functional;

import clojure.cljtest.multi.MultiExpLnTests;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ClojureFunctionalExpLnTest extends ClojureFunctionalExpressionTest {
    protected ClojureFunctionalExpLnTest(final boolean testMulti) {
        super(new MultiExpLnTests(testMulti));
    }

    public static void main(final String... args) {
        new ClojureFunctionalExpLnTest(mode(args, ClojureFunctionalExpLnTest.class)).run();
    }
}