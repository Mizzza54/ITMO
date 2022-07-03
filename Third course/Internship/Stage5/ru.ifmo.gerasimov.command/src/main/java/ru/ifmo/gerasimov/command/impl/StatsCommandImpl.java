package ru.ifmo.gerasimov.command.impl;

import org.apache.felix.service.command.CommandProcessor;
import org.osgi.service.component.annotations.*;
import ru.ifmo.gerasimov.command.StatsCommand;
import ru.ifmo.gerasimov.core.services.StatsService;
import ru.ifmo.gerasimov.core.utils.WordStatisticPair;
import ru.ifmo.gerasimov.core.exceptions.WrappedException;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Michael Gerasimov
 */
@Component(
        service = StatsCommand.class,
        immediate = true,
        property = {
                CommandProcessor.COMMAND_SCOPE + "=news",
                CommandProcessor.COMMAND_FUNCTION + "=stats"
        }
)
public class StatsCommandImpl implements StatsCommand {
    private final static String USAGE = """
            Usage:
                stats
                stats arg [n]
            """;
    private final static String ALL = "all";
    private final static Integer DEFAULT_COUNT = 10;

    private final Map<String, StatsService> statsServices = new HashMap<>();

    @Reference(
            service = StatsService.class,
            cardinality = ReferenceCardinality.MULTIPLE,
            policy = ReferencePolicy.DYNAMIC,
            bind = "bindService",
            unbind = "unbindService"
    )
    public void bindService(StatsService statsService) {
        statsServices.put(statsService.getNewsTitle(), statsService);
    }

    public void unbindService(StatsService statsService) {
        statsServices.remove(statsService.getNewsTitle());
    }

    public String stats(String[] args) {
        if (args == null) {
            return USAGE;
        }

        StringJoiner stringJoiner = new StringJoiner("\n");
        try {
            switch (args.length) {
                case 0:
                    stringJoiner.add("The following news are currently available for selection");
                    Integer lastIndex = statsServices.keySet().stream()
                            .reduce(1,
                                    (idx, x) ->
                                    {
                                        stringJoiner.add(String.format("%d) %s", idx, x));
                                        return ++idx;
                                    },
                                    (idx, x) -> ++idx);
                    stringJoiner.add(String.format("%d) %s", lastIndex, "all"));
                    return stringJoiner.toString();
                case 1:
                    return getStats(args, stringJoiner, DEFAULT_COUNT);
                case 2:
                    try {
                        Integer count = Integer.parseInt(args[1]);
                        return getStats(args, stringJoiner, count);
                    } catch (NumberFormatException e) {
                        return "Invalid argument: second argument must be number!\n" + USAGE;
                    }
                default:
                    return USAGE;
            }
        } catch (WrappedException e) {
            e.printStackTrace();
            return e.getMessage();
        }
    }

    private String getStats(String[] args, StringJoiner stringJoiner, Integer defaultCount) throws WrappedException {
        if (statsServices.containsKey(args[0])) {
            List<String> words = statsServices
                    .get(args[0])
                    .getMostFrequentlyWords(defaultCount)
                    .stream()
                    .map(WordStatisticPair::toString)
                    .collect(Collectors.toList());
            words.forEach(stringJoiner::add);
            return stringJoiner.toString();
        } else {
            if (ALL.equals(args[0])) {
                return getGeneralStats(defaultCount);
            } else {
                return "News not found!";
            }
        }
    }

    private String getGeneralStats(Integer count) throws WrappedException {
        StringJoiner stringJoiner = new StringJoiner("\n");
        List<WordStatisticPair> pairs = new ArrayList<>();
        for (StatsService elem : statsServices.values()) {
            pairs.addAll(elem.getMostFrequentlyWords(count));
        }
        pairs
                .stream()
                .sorted(Comparator.reverseOrder())
                .map(Objects::toString)
                .limit(count)
                .collect(Collectors.toList())
                .forEach(stringJoiner::add);
        return stringJoiner.toString();
    }
}
