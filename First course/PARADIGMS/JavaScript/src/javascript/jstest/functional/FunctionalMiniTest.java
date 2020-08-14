package javascript.jstest.functional;

import javascript.jstest.*;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class FunctionalMiniTest extends FunctionalExpressionTest {
    protected FunctionalMiniTest(final Language language, final boolean testParsing) {
        super(new JSEngine("functionalMiniExpression.js", ""), language, testParsing);
    }

    public static void main(final String... args) {
        test(FunctionalMiniTest.class, FunctionalMiniTest::new, args, new AbstractTests() {
            protected final AbstractExpression vx = variable("x", 0);
            {
                tests(c(10), vx);
            }
        });
    }
}
