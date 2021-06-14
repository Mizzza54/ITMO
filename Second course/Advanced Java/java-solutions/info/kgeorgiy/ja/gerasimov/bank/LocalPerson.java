package info.kgeorgiy.ja.gerasimov.bank;


import java.io.Serializable;
import java.util.Map;

/**
 * @author Michael Gerasimov
 * start: 16.05.2021
 * @version -
 */
public class LocalPerson extends CommonPerson implements Person, Serializable {
    public LocalPerson(final String firstName, final String lastName, final int passport, final Map<String, Account> accounts) {
        super(firstName, lastName, passport, accounts);
    }

    @Override
    public Account addAccount(final String id) {
        return getAccounts().putIfAbsent(id, new CommonAccount(Integer.toString(getPassport()), id));
    }
}
