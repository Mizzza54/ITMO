package info.kgeorgiy.ja.gerasimov.bank;
import org.junit.runner.JUnitCore;
import org.junit.runner.Result;

/**
 * @author Michael Gerasimov
 * start: 31.05.2021
 * @version -
 */
public class CustomTester {
    public static void main(String[] args) {
        final Result result = new JUnitCore().run(BankTests.class);
        System.err.println(result.getRunCount());
        System.err.println("exit code: " + (result.wasSuccessful() ? 0 : 1));
        System.exit(result.wasSuccessful() ? 0 : 1);
    }

}
