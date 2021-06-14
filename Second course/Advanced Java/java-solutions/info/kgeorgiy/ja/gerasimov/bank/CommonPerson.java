package info.kgeorgiy.ja.gerasimov.bank;

import java.io.Serializable;
import java.rmi.RemoteException;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author Michael Gerasimov
 * start: 23.05.2021
 * @version -
 */
public abstract class CommonPerson implements Person, Serializable {
    private final String firstName;
    private final String lastName;
    private final int passport;
    private final Map<String, Account> accounts;

    public CommonPerson(final String firstName, final String lastName, final int passport, final Map<String, Account> accounts) {
        this.firstName = firstName;
        this.lastName = lastName;
        this.passport = passport;
        this.accounts = new ConcurrentHashMap<>(accounts);
    }

    public CommonPerson(final String firstName, final String lastName, final int passport) {
        this(firstName, lastName, passport, new ConcurrentHashMap<>());
    }

    @Override
    public String getFirstName() {
        return firstName;
    }

    @Override
    public String getLastName() {
        return lastName;
    }

    @Override
    public int getPassport() {
        return passport;
    }

    @Override
    public Account getAccount(String id) {
        return accounts.get(id);
    }

    @Override
    public Set<String> getAllAccountsID() {
        return accounts.keySet();
    }

    @Override
    public Map<String, Account> getAccounts() {
        return accounts;
    }

    @Override
    public abstract Account addAccount(String id) throws RemoteException;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CommonPerson that = (CommonPerson) o;

        if (passport != that.passport) return false;
        if (!Objects.equals(firstName, that.firstName)) return false;
        if (!Objects.equals(lastName, that.lastName)) return false;
        return Objects.equals(accounts, that.accounts);
    }

    @Override
    public int hashCode() {
        int result = firstName != null ? firstName.hashCode() : 0;
        result = 31 * result + (lastName != null ? lastName.hashCode() : 0);
        result = 31 * result + passport;
        result = 31 * result + (accounts != null ? accounts.hashCode() : 0);
        return result;
    }
}
