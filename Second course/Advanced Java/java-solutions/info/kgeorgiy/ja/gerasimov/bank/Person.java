package info.kgeorgiy.ja.gerasimov.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.Map;
import java.util.Set;

/**
 * @author Michael Gerasimov
 * start: 16.05.2021
 * @version -
 */
public interface Person extends Remote {
    String getFirstName() throws RemoteException;

    String getLastName() throws RemoteException;

    int getPassport() throws RemoteException;

    Account getAccount(final String id) throws RemoteException;

    Set<String> getAllAccountsID() throws RemoteException;

    Map<String, Account> getAccounts() throws RemoteException;

    Account addAccount(String id) throws RemoteException;
}
