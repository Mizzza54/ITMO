import java.util.Arrays;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Bank thread-safe implementation.
 *
 * @author Герасимов Михаил
 */
public class BankImpl implements Bank {
    /**
     * An array of accounts by index.
     */
    private final Account[] accounts;

    /**
     * Creates new bank instance.
     * @param n the number of accounts (numbered from 0 to n-1).
     */
    public BankImpl(final int n) {
        accounts = new Account[n];
        for (int i = 0; i < n; i++) {
            accounts[i] = new Account();
        }
    }

    @Override
    public int getNumberOfAccounts() {
        return accounts.length;
    }

    /**
     * Returns thread-safe current amount in the specified account.
     *
     * @param index account index from 0 to [n][numberOfAccounts]-1.
     * @return amount in account.
     * @throws IndexOutOfBoundsException when index is invalid account index.
     */
    @Override
    public long getAmount(final int index) {
        if (!isValidIndex(index)) {
            throw new IndexOutOfBoundsException("Invalid account index");
        }

        accounts[index].lock();
        long amount;
        try {
            amount = accounts[index].getAmount();
        } finally {
            accounts[index].unlock();
        }
        return amount;
    }

    /**
     * Returns thread-safe total amount deposited in this bank.
     *
     * @return summary amount of all account.
     */
    @Override
    public long getTotalAmount() {
        Arrays.stream(accounts).forEach(Account::lock);
        long sum;
        try {
            sum = Arrays.stream(accounts).map(Account::getAmount).reduce(Long::sum).orElse(-1L);
        } finally {
            Arrays.stream(accounts).forEach(Account::unlock);
        }
        return sum;
    }

    /**
     * Deposits specified amount to account with thread-safe.
     *
     * @param index account index from 0 to [n][numberOfAccounts]-1.
     * @param amount positive amount to deposit.
     * @return resulting amount in account.
     * @throws IllegalArgumentException when amount <= 0.
     * @throws IndexOutOfBoundsException when index is invalid account index.
     * @throws IllegalStateException when deposit will overflow account above [MAX_AMOUNT].
     */
    @Override
    public long deposit(final int index, final long amount) {
        if (!isValidIndex(index)) {
            throw new IllegalArgumentException("Invalid account index");
        }
        if (amount <= 0) {
            throw new IllegalArgumentException("Invalid amount: " + amount);
        }

        accounts[index].lock();
        long newAmount;
        try {
            if (amount > MAX_AMOUNT || accounts[index].getAmount() + amount > MAX_AMOUNT) {
                throw new IllegalStateException("Overflow");
            }
            newAmount = accounts[index].deposit(amount);
        } finally {
            accounts[index].unlock();
        }

        return newAmount;
    }

    /**
     * Withdraws specified amount from account with thread-safe.
     *
     * @param index account index from 0 to [n][numberOfAccounts]-1.
     * @param amount positive amount to withdraw.
     * @return resulting amount in account.
     * @throws IllegalArgumentException when amount <= 0.
     * @throws IndexOutOfBoundsException when index is invalid account index.
     * @throws IllegalStateException when account does not enough to withdraw.
     */
    @Override
    public long withdraw(final int index, final long amount) {
        if (!isValidIndex(index)) {
            throw new IllegalArgumentException("Invalid account index");
        }
        if (amount <= 0) {
            throw new IllegalArgumentException("Invalid amount: " + amount);
        }

        accounts[index].lock();
        long newAmount;
        try {
            if (accounts[index].getAmount() - amount < 0) {
                throw new IllegalStateException("Underflow");
            }
            newAmount = accounts[index].withdraw(amount);
        } finally {
            accounts[index].unlock();
        }

        return newAmount;
    }

    /**
     * Transfers specified amount from one account to another account.
     *
     * @param fromIndex account index to withdraw from.
     * @param toIndex account index to deposit to.
     * @param amount positive amount to transfer.
     * @throws IllegalArgumentException when amount <= 0 or fromIndex == toIndex.
     * @throws IndexOutOfBoundsException when account indices are invalid.
     * @throws IllegalStateException when there is not enough funds in source account or too much in target one.
     */
    @Override
    public void transfer(final int fromIndex, final int toIndex, final long amount) {
        if (!isValidIndex(fromIndex) || !isValidIndex(toIndex)) {
            throw new IllegalArgumentException("Invalid account index");
        }
        if (amount <= 0) {
            throw new IllegalArgumentException("Invalid amount: " + amount);
        }
        if (fromIndex == toIndex) {
            throw new IllegalArgumentException("fromIndex == toIndex");
        }

        accounts[Math.min(fromIndex, toIndex)].lock();
        accounts[Math.max(fromIndex, toIndex)].lock();
        try {
            Account from = accounts[fromIndex];
            Account to = accounts[toIndex];
            if (amount > from.amount) {
                throw new IllegalStateException("Underflow");
            } else if (amount > MAX_AMOUNT || to.amount + amount > MAX_AMOUNT) {
                throw new IllegalStateException("Overflow");
            }

            from.withdraw(amount);
            to.deposit(amount);
        } finally {
            accounts[fromIndex].unlock();
            accounts[toIndex].unlock();
        }
    }

    private boolean isValidIndex(final int index) {
        return index < getNumberOfAccounts() && index >= 0;
    }

    /**
     * Private account data structure.
     */
    static class Account {
        private final ReentrantLock lock;
        /**
         * Amount of funds in this account.
         */
        private long amount;

        private Account() {
            this.lock = new ReentrantLock();
        }

        private long deposit(final long amount) {
            return this.amount += amount;
        }

        private long withdraw(final long amount) {
            return this.amount -= amount;
        }

        private long getAmount() {
            return amount;
        }

        private void lock() {
            lock.lock();
        }

        private void unlock() {
            lock.unlock();
        }
    }
}
