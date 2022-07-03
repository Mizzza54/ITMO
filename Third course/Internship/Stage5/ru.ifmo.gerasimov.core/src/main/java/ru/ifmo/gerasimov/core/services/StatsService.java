package ru.ifmo.gerasimov.core.services;

import ru.ifmo.gerasimov.core.exceptions.WrappedException;
import ru.ifmo.gerasimov.core.utils.WordStatisticPair;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;

/**
 * @author Michael Gerasimov
 */
public interface StatsService {

    String getNewsTitle();

    URL getRSSFeedURL() throws WrappedException;

    List<String> getAllWords() throws WrappedException;

    List<WordStatisticPair> getMostFrequentlyWords(Integer count) throws WrappedException;
}
