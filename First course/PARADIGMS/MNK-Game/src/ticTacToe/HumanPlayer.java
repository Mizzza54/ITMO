package ticTacToe;

import java.io.PrintStream;
import java.util.Scanner;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class HumanPlayer implements Player {
    private final PrintStream out;
    private final Scanner in;
    public final int id;

    public HumanPlayer(final PrintStream out, final Scanner in, int id) {
        this.out = out;
        this.in = in;
        this.id = id;
    }

    public HumanPlayer(int id) {
        this(System.out, new Scanner(System.in), id);
    }

    @Override
    public Move move(final Position position, final Cell cell) {
        boolean flag = true;
        while (flag) {
            while (true) {
                flag = false;
                out.println("Position");
                out.println(position);
                out.println(cell + "'s move");
                out.println("Enter row and column");
                String s = in.nextLine();
                int[] numbers = isInteger(s);
                if (numbers.length != 1) {
                    final Move move = new Move(numbers[0], numbers[1], cell);
                    if (position.isValid(move)) {
                        return move;
                    }
                    final int row = move.getRow();
                    final int column = move.getColumn();
                    out.println("Move " + move + " is invalid");
                } else {
                    out.println("Move is invalid");
                    flag = true;
                }
            }
        }
        return null;
    }

    public static int[] isInteger(String s) throws NumberFormatException {
        try {
            String[] tmp = s.split(" ");
            int[] digits = new int[tmp.length];
            for (int i = 0; i < tmp.length; i++) {
                digits[i] = Integer.parseInt(tmp[i]);
            }
            return digits;
        } catch (NumberFormatException e) {
            int[] digits = new int[1];
            digits[0] = -1;
            return digits;
        }
    }

    @Override
    public int id() {
        return id;
    }
}
