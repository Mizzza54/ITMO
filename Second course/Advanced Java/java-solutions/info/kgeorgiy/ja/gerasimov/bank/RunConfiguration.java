package info.kgeorgiy.ja.gerasimov.bank;

/**
 * @author Michael Gerasimov
 * start: 23.05.2021
 * @version -
 */
public class RunConfiguration {
    private final String firstName;
    private final String lastName;
    private final int passport;
    private final int accountID;
    private final int change;
    private final String id;
    private final static int NUMBER_OF_ARGUMENTS = 5;

    RunConfiguration(String[] args) throws IllegalArgumentException {
        if (!isValidArgs(args)) {
            throw new IllegalArgumentException("Usage: Client firstName, lastName, passport, accountID, change");
        }

        firstName = args[0];
        lastName = args[1];
        try {
            passport = Integer.parseInt(args[2]);
            accountID = Integer.parseInt(args[3]);
            change = Integer.parseInt(args[4]);
        } catch (NumberFormatException e) {
            throw new NumberFormatException();
        }

        id = String.format("%s:%s", passport, accountID);
    }

    private static boolean isValidArgs(final String[] args) {
        if (args == null || args.length != NUMBER_OF_ARGUMENTS) {
            return false;
        }

        for (String str: args) {
            if (str == null) {
                return false;
            }
        }
        return true;
    }

    public String getFirstName() {
        return firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public int getPassport() {
        return passport;
    }

    public int getAccountID() {
        return accountID;
    }

    public int getChange() {
        return change;
    }

    public String getAccountIdentifier() {
        return id;
    }
}
