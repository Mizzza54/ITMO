package ticTacToe;

import java.util.ArrayList;
import java.util.Scanner;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class Main {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int n = 0, m = 0, k = 0, players_count = 2, round_count = 0;
        int[] digits;
        String[] tmp = new String[1];
        tmp[0] = "qwerty";
        ArrayList<Player> id = new ArrayList<>();
        boolean bool = true;
        while (bool) {
            System.out.println("Введите через пробел n, m, и k: (n ~ кол-во строк; m ~ кол-во столбцов)");
            digits = inputMNK(scanner.nextLine());
            bool = false;
            if (digits.length == 1) {
                System.out.println("Неверные NMK");
                bool = true;
                continue;
            }
            n = digits[0];
            m = digits[1];
            k = digits[2];
            if (k > n || k > m || k == 1) {
                System.out.println("Неверное K");
                bool = true;
                continue;
            }
        }
        bool = true;
        while (bool) {
            bool = false;
            System.out.println("Введите через пробел кол-во игроков и кол-во раундов:");
            digits = inputMNK(scanner.nextLine());
            if (digits.length == 1) {
                System.out.println("Неверные кол-во игроков и кол-во раундов");
                bool = true;
                continue;
            }
            players_count = digits[0];
            round_count = digits[1];
            if (round_count == 0 || players_count == 1) {
                System.out.println("Неверные кол-во игроков и кол-во раундов");
                bool = true;
                continue;
            }
            System.out.println("Введите через пробел типы игроков: (H ~ human; R ~ Random; S ~ Sequential)");
            String line = scanner.nextLine();
            tmp = line.split(" ");
            if (players_count != tmp.length) {
                System.out.println("Неверное кол-во игроков");
                bool = true;
                continue;
            }
            for (int i = 0; i < players_count; i++) {
                switch (tmp[i]) {
                    case "H":
                        id.add(new HumanPlayer(i));
                        break;
                    case "R":
                        id.add(new RandomPlayer(n, m, i));
                        break;
                    case "S":
                        id.add(new SequentialPlayer(n, m, i));
                        break;
                    default:
                        System.out.println("Неверные игроки");
                        bool = true;
                        break;
                }
            }
        }
        Score score = new Score(id, round_count);
        for (int i = 0; i < round_count; i++) {
            for (int j = 0; j < players_count; j++) {
                for (int p = j + 1; p < players_count; p++) {
                    final Game game = new Game(false, id.get(j), id.get(p), score);
                    int result;
                    result = game.play(new mnkBoard(n, m, k));
                }
            }
            score.ResultOfRound();
            score.getRoundScore(i+1);
            System.out.println("---------------------");
        }
        score.getPoints();
    }

    public static int[] inputMNK(String s) throws NumberFormatException {
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
}
