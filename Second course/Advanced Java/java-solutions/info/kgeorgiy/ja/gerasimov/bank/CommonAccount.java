package info.kgeorgiy.ja.gerasimov.bank;

import java.io.Serializable;
import java.rmi.RemoteException;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * @author Michael Gerasimov
 * // :NOTE: ??
 * start: 30.05.2021
 * @version -
 */
public class CommonAccount implements Account, Serializable {
    private final String id;
    private final String passport;
    private final AtomicInteger amount;


    public CommonAccount(final String passport, final String id, final int amount) throws RemoteException {
        this.passport = passport;
        this.id = id;
        this.amount = new AtomicInteger(amount);
    }

    // :NOTE: cp
    public CommonAccount(final String passport, final String id) {
        this.passport = passport;
        this.id = id;
        this.amount = new AtomicInteger(0);
    }

    @Override
    public String getId() throws RemoteException {
        return String.format("%s:%s", passport, id);
    }

    @Override
    public int getAmount() throws RemoteException {
        System.out.println("Getting amount of money for account " + id);
        return amount.get();
    }

    @Override
    public void setAmount(final int amount) throws RemoteException {
        System.out.println("Setting amount of money for account " + id);
        this.amount.set(amount);
    }

    @Override
    public synchronized void addAmount(final int amount) throws RemoteException {
        System.out.println("Adding amount of money for account " + id);
        this.amount.addAndGet(amount);
    }
}
