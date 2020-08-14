package ticTacToe;

import java.util.Arrays;
import java.util.Map;

/**
 * @author Michale Gerasimov
 * start: 26.11.2019
 * @version 10.12.2019
 */
public class mnkBoard implements Board, Position {
    private static final Map<Cell, Character> SYMBOLS = Map.of(
            Cell.X, 'X',
            Cell.O, 'O',
            Cell.E, '.'
    );

    private final Cell[][] cells;
    private Cell turn;
    private final int m;
    private final int n;
    private final int k;


    public mnkBoard(final int m, final int n, final int k) {
        this.m = m;
        this.n = n;
        this.k = k;
        this.cells = new Cell[m][n];
        for (Cell[] row : cells) {
            Arrays.fill(row, Cell.E);
        }
        turn = Cell.X;
    }

    @Override
    public Position getPosition() {
        return this;
    }

    @Override
    public Cell getCell() {
        return turn;
    }

    public int Counter(int row, int column, int dx, int dy) {
        int count = 0;
        for (int x = row, y = column; x < m && x >= 0 && y < n && y >= 0 && cells[x][y] == turn; x += dx, y += dy) {
            count++;
        }
        return count;
    }

    @Override
    public Result makeMove(final Move move) {
        if (!isValid(move)) {
            return Result.LOSE;
        }
        cells[move.getRow()][move.getColumn()] = move.getValue();

        int empty = 0;
        int inRow = Counter(move.getRow(), move.getColumn(), 1, 0) + Counter(move.getRow(), move.getColumn(), -1, 0) - 1;
        int inColumn = Counter(move.getRow(), move.getColumn(), 0, 1) + Counter(move.getRow(), move.getColumn(), 0, -1) - 1;
        int inDiag1 = Counter(move.getRow(), move.getColumn(), 1, 1) + Counter(move.getRow(), move.getColumn(), -1, -1) - 1;
        int inDiag2 = Counter(move.getRow(), move.getColumn(), 1, -1) + Counter(move.getRow(), move.getColumn(), -1, 1) - 1;
        if (inRow == k || inColumn == k || inDiag1 == k || inDiag2 == k) {
            return Result.WIN;
        }

        for (int u = 0; u < m; u++) {
            for (int v = 0; v < n; v++) {
                if ((u < m) && (v < n) && (cells[u][v] == Cell.E)) {
                    empty++;
                }
            }
        }
        if (empty == 0) {
            return Result.DRAW;
        }

        turn = turn == Cell.X ? Cell.O : Cell.X;
        return Result.UNKNOWN;
    }

    @Override
    public boolean isValid(final Move move) {
        return 0 <= move.getRow() && move.getRow() < m
                && 0 <= move.getColumn() && move.getColumn() < n
                && cells[move.getRow()][move.getColumn()] == Cell.E
                && turn == getCell();
    }

    @Override
    public Cell getCell(final int r, final int c) {
        return cells[r][c];
    }

    @Override
    public String toString() {
        final StringBuilder sbg = new StringBuilder(" ");
        for (int i = 0; i < n; i++) {
            sbg.append(String.valueOf(i));
        }
        for (int r = 0; r < m; r++) {
            sbg.append("\n");
            sbg.append(r);
            for (int c = 0; c < n; c++) {
                sbg.append(SYMBOLS.get(cells[r][c]));
            }
        }
        return sbg.toString();
    }
}
