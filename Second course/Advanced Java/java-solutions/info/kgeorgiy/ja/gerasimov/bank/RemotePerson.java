package info.kgeorgiy.ja.gerasimov.bank;

import java.io.UncheckedIOException;
import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * @author Michael Gerasimov
 * start: 16.05.2021
 * @version -
 */
// :NOTE: ??
public class RemotePerson extends CommonPerson implements Person, Remote {
    // :NOTE: Ассиметричный интерфейс
    public RemotePerson(final String firstName, final String lastName, final int passport) throws RemoteException {
        super(firstName, lastName, passport);
    }

    @Override
    public Account addAccount(final String id) throws RemoteException {
        try {
            // :NOTE: Изменяемая коллекция
            return getAccounts().computeIfAbsent(id, x -> new CommonAccount(Integer.toString(getPassport()), id));
        } catch (final UncheckedIOException e) {
            throw (RemoteException) e.getCause();
        }
    }
}
