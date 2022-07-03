package ru.ifmo.gerasimov.core.services;

import ru.ifmo.gerasimov.core.exceptions.WrappedException;
import ru.ifmo.gerasimov.core.parser.RSSParser;
import ru.ifmo.gerasimov.core.services.StatsService;
import ru.ifmo.gerasimov.core.utils.WordStatisticPair;

import java.util.List;

/**
 * @author Michael Gerasimov
 */
public abstract class AbstractStatsService implements StatsService {
    @Override
    public List<String> getAllWords() throws WrappedException {
        return RSSParser.getAllWords(getRSSFeedURL());
    }

    @Override
    public List<WordStatisticPair> getMostFrequentlyWords(Integer count) throws WrappedException {
        return RSSParser.getMostFrequentlyWords(getRSSFeedURL(), count);
    }
}
