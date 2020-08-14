package ticTacToe;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class SequentialPlayer implements Player {
    private final int row;
    private final int column;
    public final int id;

    public SequentialPlayer(int row, int column, int id) {
        this.row = row;
        this.column = column;
        this.id = id;
    }

    @Override
    public Move move(final Position position, final Cell cell) {
        for (int r = 0; r < row; r++) {
            for (int c = 0; c < column; c++) {
                final Move move = new Move(r, c, cell);
                if (position.isValid(move)) {
                    return move;
                }
            }
        }
        throw new IllegalStateException("No valid moves");
    }

    @Override
    public int id() {
        return id;
    }
}
