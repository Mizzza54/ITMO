package ru.ifmo.gerasimov.core.utils;

/**
 * @author Michael Gerasimov
 */
public class WordStatisticPair implements Comparable<WordStatisticPair> {
    private final String word;
    private final Integer count;

    public WordStatisticPair(String word, Integer count) {
        this.word = word;
        this.count = count;
    }

    public String getWord() {
        return word;
    }

    public Integer getCount() {
        return count;
    }

    @Override
    public int compareTo(WordStatisticPair o) {
        return Integer.compare(this.getCount(), o.getCount());
    }

    @Override
    public String toString() {
        return String.format("word='%s', count=%d", word, count);
    }
}