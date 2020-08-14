package ticTacToe;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * @author Michale Gerasimov
 * start: 02.12.2019
 * @version 03.12.2019
 */
public class Score {
    private final ArrayList<Player> id;
    private final int round_count;
    public int round_now;
    private Integer[][] results;
    private HashMap<Integer, Integer[][]> results_of_rounds = new HashMap<>();
    private int games_count; // Игр в одном раунде
    private int[] points;

    public Score(ArrayList<Player> id, int round_count) {
        this.id = id;
        this.round_now = 1;
        this.round_count = round_count;
        this.results = new Integer[id.size()][id.size()];
        this.points = new int[id.size()];
        for (int i = 0; i < id.size(); i++) {
            for (int j = 0; j < id.size(); j++) {
                results[i][j] = -2;
            }
        }
        this.games_count = 0;
        for (int i = 1; i < id.size(); i++) {
            this.games_count += i;
        }
        // results - таблица побед и поражений для 1-ого раунда, где i и j - id игроков которые играли.
        // Клетка хранит WIN(1), LOSE(0) или DRAW(-1) (ещё -2 если i = j). Измерение относительно игрока i
    }

    public void getRoundScore(int round) {
        Integer[][] tmp = results_of_rounds.get(round);
        for (int i = 0; i < tmp.length; i++) {
            for (int j = 0; j < tmp[i].length; j++) {
                switch (tmp[i][j]) {
                    case 1:
                        System.out.print("W");
                        break;
                    case 0:
                        System.out.print("L");
                        break;
                    case -1:
                        System.out.print("D");
                        break;
                    case -2:
                        System.out.print("-");
                        break;
                }
            }
            System.out.println();
        }
    }

    public void ResultOfGame(Player player1, Player player2, int result) {
        //int tmp1 = id.indexOf(player1);
        int tmp1 = player1.id();
        int tmp2 = player2.id();
        if (result == 1) {
            results[tmp1][tmp2] = 1;
            results[tmp2][tmp1] = 0;
        } else if (result == -1) {
            results[tmp1][tmp2] = -1;
            results[tmp2][tmp1] = -1;
        }
    }

    public void ResultOfRound() {
        results_of_rounds.put(round_now, results);
        round_now++;
        for (int i = 0; i < id.size(); i++) {
            for (int j = 0; j < id.size(); j++) {
                switch (results[i][j]) {
                    case 1:
                        points[i] += 3;
                        break;
                    case -1:
                        points[i] += 1;
                        break;
                    case 0:
                        points[i] += 0;
                        break;
                }
            }
        }
    }

    public void getPoints() {
        for (int i = 0; i < id.size(); i++) {
            System.out.println((i+1) + ") " + points[i] );
        }
    }
}
