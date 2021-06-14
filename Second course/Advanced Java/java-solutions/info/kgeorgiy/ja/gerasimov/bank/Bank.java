package info.kgeorgiy.ja.gerasimov.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Bank extends Remote {
    /**
     * Creates a new account with specified identifier if it is not already exists.
     * @param id account id
     * @return created or existing account.
     */
    Account createAccount(Person person, String id) throws RemoteException;

    /**
     * Returns account by identifier.
     * @param id account id
     * @return account with specified identifier or {@code null} if such account does not exists.
     */
    Account getAccount(Person person, String id) throws RemoteException;

    Person createPerson(String firstName, String lastName, int passport) throws RemoteException;

    Person getLocalPerson(int passport) throws RemoteException;

    Person getRemotePerson(int passport) throws RemoteException;

    Account getRemoteAccountByID(String id) throws RemoteException;

    Account getLocalAccountByID(String id) throws RemoteException;
}
