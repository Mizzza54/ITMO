package ticTacToe;

import java.util.Random;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class RandomPlayer implements Player {
    private final Random random;
    private final int row;
    private final int column;
    public final int id;

    public RandomPlayer(final Random random, int row, int column, int id) {
        this.random = random;
        this.row = row;
        this.column = column;
        this.id = id;
    }

    public RandomPlayer(int row, int column, int id) {
        this.random = new Random();
        this.row = row;
        this.column = column;
        this.id = id;
    }

    @Override
    public Move move(final Position position, final Cell cell) {
        while (true) {
            int r = random.nextInt(row);
            int c = random.nextInt(column);
            final Move move = new Move(r, c, cell);
            if (position.isValid(move)) {
                return move;
            }
        }
    }

    @Override
    public int id() {
        return id;
    }
}
