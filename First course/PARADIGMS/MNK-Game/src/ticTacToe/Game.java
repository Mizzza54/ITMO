package ticTacToe;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class Game {
    private final boolean log;
    private final Player player1, player2;
    public Score score;

    public Game(final boolean log, final Player player1, final Player player2, Score score) {
        this.log = log;
        this.player1 = player1;
        this.player2 = player2;
        this.score = score;
    }

    public int play(Board board) {
        while (true) {
            final int result1 = move(board, player1, 1);
            if (result1 != -2) {
                score.ResultOfGame(player1, player2, result1);
                return result1;
            }
            final int result2 = move(board, player2, 2);
            if (result2 != -2) {
                score.ResultOfGame(player2, player1, result2);
                return result2;
            }
        }
    }

    private int move(final Board board, final Player player, final int no) {
        final Move move = player.move(board.getPosition(), board.getCell());
        final Result result = board.makeMove(move);
        log("Player " + no + " move: " + move);
        log("Position:\n" + board);
        if (result == Result.WIN) {
            log("Player " + no + " won");
            return 1;
        } else if (result == Result.LOSE) {
            log("Player " + no + " lose");
            return 0;
        } else if (result == Result.DRAW) {
            log("Draw");
            return -1;
        } else {
            return -2;
        }
    }

    private void log(final String message) {
        if (log) {
            System.out.println(message);
        }
    }
}
