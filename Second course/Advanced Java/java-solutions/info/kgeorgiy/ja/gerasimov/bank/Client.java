package info.kgeorgiy.ja.gerasimov.bank;

import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;

public final class Client {
    /** Utility class. */
    private Client() {}

    /**
     * Usage: Client firstName, lastName, passport, accountID, change.
     * <p>Command line arguments: first name, last name, passport number of an individual, account number, change of the account amount.
     * @param args arguments from CLI
     * @throws RemoteException throw RemoteException
     */
    public static void main(final String... args) throws RemoteException {
        final Bank bank;
        try {
            bank = (Bank) Naming.lookup("//localhost/bank");
        } catch (final NotBoundException e) {
            System.out.println("Bank is not bound");
            return;
        } catch (final MalformedURLException e) {
            System.out.println("Bank URL is invalid");
            return;
        }

        RunConfiguration configuration;
        try {
            configuration = new RunConfiguration(args);
        } catch (IllegalArgumentException e) {
            System.err.println("CLI arguments is invalid.");
            return;
        }

        Person person = bank.getRemotePerson(configuration.getPassport());
        if (person == null) {
            System.out.println("Creating person");
            person = bank.createPerson(configuration.getFirstName(), configuration.getLastName(), configuration.getPassport());
        } else {
            System.out.println("Person already exists");
        }

        Account account = bank.getAccount(person, configuration.getAccountIdentifier());
        if (account == null) {
            System.out.println("Creating account");
            account = bank.createAccount(person, configuration.getAccountIdentifier());
        } else {
            System.out.println("Account already exists");
        }

        System.out.println("Account id: " + account.getId());
        System.out.println("Money: " + account.getAmount());
        System.out.println("Adding money");
        account.setAmount(account.getAmount() + configuration.getChange());
        System.out.println("Money: " + account.getAmount());
    }
}
