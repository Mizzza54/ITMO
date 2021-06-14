package info.kgeorgiy.ja.gerasimov.bank;

import java.io.UncheckedIOException;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Function;

public class RemoteBank implements Bank {
    private final int port;
    private final ConcurrentMap<Integer, Person> persons = new ConcurrentHashMap<>();

    public RemoteBank(final int port) {
        this.port = port;
    }

    @Override
    public Account createAccount(final Person person, final String id) throws RemoteException {
        System.out.println("Creating account " + id);
        final Account account = new CommonAccount(Integer.toString(person.getPassport()), id);
        if (person.getAccount(id) == null) {
            UnicastRemoteObject.exportObject(account, port);
            person.addAccount(id);
            return account;
        } else {
            return getAccount(person, id);
        }
    }

    private static Account getAccountById(final String id, Function<Integer, Person> function) throws RemoteException {
        String[] parts = id.split(":");
        try {
            // :NOTE: IAOBE
            return function.apply(Integer.parseInt(parts[0])).getAccount(parts[1]);
        } catch (NumberFormatException e) {
            System.err.println("Invalid id of account");
        }
        return null;
    }

    public Account getRemoteAccountByID(final String id) throws RemoteException {
        return getAccountById(id, this::getRemotePerson);
    }

    public Account getLocalAccountByID(final String id) throws RemoteException {
        return getAccountById(id, (i) -> {
            try {
                return getLocalPerson(i);
            } catch (RemoteException e) {
                throw new UncheckedIOException(e);
            }
        });
    }

    @Override
    public Account getAccount(final Person person, final String id) throws RemoteException {
        System.out.println("Retrieving account " + id);
        return person.getAccount(id);
    }

    @Override
    public Person createPerson(final String firstName, final String lastName, final int passport) throws RemoteException {
        System.out.println("Creating person " + firstName + " " + lastName + " " + passport);
        final Person person = new RemotePerson(firstName, lastName, passport);
        persons.computeIfAbsent(passport, x -> {
            try {
                UnicastRemoteObject.exportObject(person, port);
                return new RemotePerson(firstName, lastName, passport);
            } catch (RemoteException e) {
                throw new UncheckedIOException(e);
            }
        });
        return persons.get(passport);
    }

    @Override
    public Person getLocalPerson(final int passport) throws RemoteException {
        final Person person = persons.get(passport);
        if (person == null) {
            return null;
        }
        final Map<String, Account> personAccounts = new ConcurrentHashMap<>();
        for (Map.Entry<String, Account> elem : person.getAccounts().entrySet()) {
            personAccounts.put(elem.getKey(), new CommonAccount(Integer.toString(person.getPassport()), elem.getValue().getId(), elem.getValue().getAmount()));
        }
        return new LocalPerson(person.getFirstName(),
                person.getLastName(),
                person.getPassport(),
                personAccounts
        );
    }

    @Override
    public Person getRemotePerson(final int passport) {
        System.out.println("Retrieving person " + passport);
        return persons.get(passport);
    }
}
