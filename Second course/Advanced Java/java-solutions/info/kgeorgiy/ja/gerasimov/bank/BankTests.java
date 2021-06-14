package info.kgeorgiy.ja.gerasimov.bank;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.concurrent.*;

import static org.junit.Assert.*;

public class BankTests {

    private static Bank bank;
    private static Registry registry;
    private final static int DEFAULT_PORT = 8888;


    private static Person simpleRemotePerson(final int id) {
        try {
            return new RemotePerson("firstName" + id, "lastName" + id, id);
        } catch (final RemoteException e) {
            // Ignore
        }
        return null;
    }

    private static Person createPersonAtBank(final int id) throws RemoteException {
        final Person person = simpleRemotePerson(id);
        assert person != null;
        return bank.createPerson(person.getFirstName(), person.getLastName(), person.getPassport());
    }

    private static void createAccountsForPerson(final Person person, final int count) throws RemoteException {
        for (int i = 0; i < count; i++) {
            bank.createAccount(person, Integer.toString(i));
        }
    }

    private static void assertPersonEquals(final Person a, final Person b) throws RemoteException {
        assertEquals(a.getLastName(), b.getLastName());
        assertEquals(a.getFirstName(), b.getFirstName());
        assertEquals(a.getPassport(), b.getPassport());
    }

    @Before
    public void startServer() throws RemoteException, NotBoundException {
        registry = LocateRegistry.createRegistry(Registry.REGISTRY_PORT);
        registry.rebind("//localhost/bank", new RemoteBank(DEFAULT_PORT));
        bank = (Bank) registry.lookup("//localhost/bank");
        System.out.println("Server started");
    }

    @After
    public void destroyServer() throws NotBoundException, RemoteException {
        registry.unbind("//localhost/bank");
        UnicastRemoteObject.unexportObject(registry, true);
        registry = null;
        bank = null;
        System.out.println("Server destroyed");
    }

    @Test
    public void getNonExistentPerson() throws RemoteException {
        assertNull(bank.getLocalPerson(Integer.MAX_VALUE));
        assertNull(bank.getLocalPerson(0));
        assertNull(bank.getLocalPerson(Integer.MIN_VALUE));
        assertNull(bank.getRemotePerson(Integer.MAX_VALUE));
        assertNull(bank.getRemotePerson(0));
        assertNull(bank.getRemotePerson(Integer.MIN_VALUE));
    }

    @Test
    public void getExistentPerson() throws RemoteException {
        final Person person = createPersonAtBank(0);
        final Person remotePerson = bank.getRemotePerson(person.getPassport());
        final Person localPerson = bank.getLocalPerson(person.getPassport());
        assertPersonEquals(remotePerson, person);
        assertPersonEquals(localPerson, person);
    }

    @Test
    public void getAccountsFromPersonWhoHasNotThem() throws RemoteException {
        final Person person = createPersonAtBank(0);
        assertTrue(person.getAllAccountsID().isEmpty());
    }

    @Test
    public void getAccountsFromPersonWhoHasThem() throws RemoteException {
        final Person person = createPersonAtBank(0);
        createAccountsForPerson(person, 100);
        assertEquals(100, person.getAllAccountsID().size());
        for (int i = 0; i < 100; i++) {
            assertEquals(String.format("%s:%s", person.getPassport(), i), person.getAccount(Integer.toString(i)).getId());
        }
    }

    @Test
    public void checkOperationWithAmount() throws RemoteException {
        final Person person = createPersonAtBank(0);
        createAccountsForPerson(person, 1);
        final Account account = bank.getAccount(person, "0");
        assertEquals(account.getAmount(), 0);
        account.setAmount(100);
        assertEquals(account.getAmount(), 100);
        account.setAmount(500);
        assertEquals(account.getAmount(), 500);
    }

    @Test
    public void testLocalVsRemoteByAmount1() throws RemoteException {
        final Person person = createPersonAtBank(0);
        createAccountsForPerson(person, 1);

        final Person localPerson = bank.getLocalPerson(0);
        final Account localAccount = bank.getAccount(localPerson, "0");

        localAccount.setAmount(100);

        assertEquals(100, localAccount.getAmount());
        assertEquals(1, localPerson.getAccounts().size());

        assertEquals(0, bank.getAccount(person, "0").getAmount());
        assertEquals(1, person.getAccounts().size());
    }

    @Test
    public void testLocalVsRemoteByAmount2() throws RemoteException {
        final Person person = createPersonAtBank(0);
        createAccountsForPerson(person, 1);

        final Account remoteAccount = bank.getAccount(person, "0");
        final Person localPersonBefore = bank.getLocalPerson(0);
        final Account localAccountBefore = localPersonBefore.getAccount("0");

        remoteAccount.setAmount(100);
        assertEquals(0, localAccountBefore.getAmount());
        assertEquals(1, localPersonBefore.getAccounts().size());
        assertEquals(100, bank.getAccount(person, "0").getAmount());
        assertEquals(1, person.getAccounts().size());

        final Person localPersonAfter = bank.getLocalPerson(0);
        final Account localAccountAfter = localPersonAfter.getAccount("0");

        remoteAccount.setAmount(200);
        assertEquals(0, localAccountBefore.getAmount());
        assertEquals(1, localPersonBefore.getAccounts().size());

        assertEquals(200, bank.getAccount(person, "0").getAmount());
        assertEquals(1, person.getAccounts().size());

        assertEquals(100, localAccountAfter.getAmount());
        assertEquals(1, localPersonAfter.getAccounts().size());
    }

    @Test
    public void parallelAddAmount() throws RemoteException {
        final Person person = createPersonAtBank(0);
        createAccountsForPerson(person, 1);
        final ExecutorService executorService = Executors.newFixedThreadPool(20);
        final Account account = bank.getAccount(person, "0");
        final Phaser phaser = new Phaser(1);
        for (int i = 0; i < 20; i++) {
            phaser.register();
            executorService.submit(() -> {
                try {
                    account.addAmount(10);
                } catch (final RemoteException e) {
                    e.printStackTrace();
                } finally {
                    phaser.arrive();
                }
            });
        }
        phaser.arriveAndAwaitAdvance();
        assertEquals(200, bank.getAccount(person, "0").getAmount());
    }

    @Test
    public void parallelCreateAccounts() throws RemoteException {
        final Person person = createPersonAtBank(0);
        createAccountsForPerson(person, 1);
        final ExecutorService executorService = Executors.newFixedThreadPool(10);
        final Phaser phaser = new Phaser(1);
        for (int i = 0; i < 10; i++) {
            final int finalI = i;
            phaser.register();
            executorService.submit(() -> {
                try {
                    bank.createAccount(person, Integer.toString(finalI));
                } catch (final RemoteException e) {
                    e.printStackTrace();
                } finally {
                    phaser.arrive();
                }
            });
        }
        phaser.arriveAndAwaitAdvance();
        assertEquals(10, person.getAccounts().size());
    }

    @Test
    public void parallelCreatePersons() throws RemoteException {
        // :NOTE: Не закрывается
        final ExecutorService executorService = Executors.newFixedThreadPool(10);
        final Phaser phaser = new Phaser(1);
        for (int i = 0; i < 10; i++) {
            final int finalI = i; // :NOTE: IntStream
            phaser.register();
            executorService.submit(() -> {
                try {
                    createPersonAtBank(finalI);
                } catch (final RemoteException e) {
                    e.printStackTrace();
                } finally {
                    phaser.arrive();
                }
            });
        }
        phaser.arriveAndAwaitAdvance();
        for (int i = 0; i < 10; i++) {
            assertNotNull(bank.getRemotePerson(i));
        }
    }

    @Test
    public void getAccountByLongId() throws RemoteException {
        final Person person = createPersonAtBank(0);
        createAccountsForPerson(person, 3);
        bank.getAccount(person, "0").setAmount(100);
        bank.getAccount(person, "1").setAmount(200);
        bank.getAccount(person, "2").setAmount(300);
        final Account localAccount1 = bank.getLocalAccountByID(String.format("%s:%s", person.getPassport(), 0));
        final Account localAccount2 = bank.getLocalAccountByID(String.format("%s:%s", person.getPassport(), 1));
        final Account localAccount3 = bank.getLocalAccountByID(String.format("%s:%s", person.getPassport(), 2));
        assertEquals(100, localAccount1.getAmount());
        assertEquals(200, localAccount2.getAmount());
        assertEquals(300, localAccount3.getAmount());
        final Account remoteAccount1 = bank.getRemoteAccountByID(String.format("%s:%s", person.getPassport(), 0));
        final Account remoteAccount2 = bank.getRemoteAccountByID(String.format("%s:%s", person.getPassport(), 1));
        final Account remoteAccount3 = bank.getRemoteAccountByID(String.format("%s:%s", person.getPassport(), 2));
        assertEquals(100, remoteAccount1.getAmount());
        assertEquals(200, remoteAccount2.getAmount());
        assertEquals(300, remoteAccount3.getAmount());
    }

}
