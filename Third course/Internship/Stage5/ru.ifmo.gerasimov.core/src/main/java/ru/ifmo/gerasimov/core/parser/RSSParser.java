package ru.ifmo.gerasimov.core.parser;

import com.rometools.rome.feed.synd.SyndEntry;
import com.rometools.rome.feed.synd.SyndFeed;
import com.rometools.rome.io.FeedException;
import com.rometools.rome.io.SyndFeedInput;
import com.rometools.rome.io.XmlReader;
import ru.ifmo.gerasimov.core.utils.WordStatisticPair;
import ru.ifmo.gerasimov.core.exceptions.WrappedException;

import java.io.IOException;
import java.net.URL;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author Michael Gerasimov
 */
public class RSSParser {

    private RSSParser() {}

    public static List<String> getAllWords(URL url) throws WrappedException {
        try {
            SyndFeedInput input = new SyndFeedInput();
            SyndFeed feed = input.build(new XmlReader(url));
            List<String> titles = feed.getEntries().stream().map(SyndEntry::getTitle).collect(Collectors.toList());
            return titles
                    .stream()
                    .flatMap(x -> Arrays.stream(x.split("[,;:.!?\\s]+")).map(String::toLowerCase))
                    .collect(Collectors.toList());
        } catch (FeedException | IOException e) {
            throw new WrappedException(e);
        }
    }

    public static Map<String, Integer> getWordsStatistic(URL url) throws WrappedException {
        List<String> words = getAllWords(url);
        return words
                .stream()
                .collect(Collectors.groupingBy(Function.identity(), Collectors.summingInt(e -> 1)));
    }

    public static List<WordStatisticPair> getMostFrequentlyWords(URL url, int count) throws WrappedException {
        return getWordsStatistic(url)
                .entrySet()
                .stream()
                .map(x -> new WordStatisticPair(x.getKey(), x.getValue()))
                .sorted(Comparator.reverseOrder())
                .limit(count)
                .collect(Collectors.toList());
    }
}
