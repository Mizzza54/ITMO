package info.kgeorgiy.ja.gerasimov.i18n;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.*;
import java.util.Arrays;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.stream.Collectors;

/**
 * @author Michael Gerasimov
 * start: 04.06.2021
 * @version -
 */
public class TextStatistics {

    enum OutputLocale {
        RU("ru_RU"),
        EN("en_US");

        private final String value;

        OutputLocale(final String value) {
            this.value = value;
        }

        public String getValue() {
            return this.value;
        }

        public static OutputLocale fromString(String value) {
            if (value != null) {
                for (OutputLocale locale : values()) {
                    if (value.equalsIgnoreCase(locale.getValue())) {
                        return locale;
                    }
                }
            }
            return null;
        }
    }

    private static final String STATISTICS_RESOURCE_BUNDLE = "info.kgeorgiy.ja.gerasimov.i18n.resources.StatisticsResourceBundle";
    private static final String USAGE_MESSAGE = "Usage: <inputLocale> <outputLocale> <inputFile> <outputFile>";
    private final Categories[] arrayCategories;
    private final String inputText;
    private final ResourceBundle resourceBundle;

    private static boolean isValidArgs(final String[] args) {
        if (args == null || args.length != 4) {
            return false;
        }
        for (String string: args) {
            if (string == null) {
                return false;
            }
        }
        return true;
    }

    public static void main(final String[] args) {
        if (!isValidArgs(args)) {
            System.err.println("Invalid args");
            System.err.println(USAGE_MESSAGE);
            return;
        }

        final String[] splitArgsInputLocale = args[0].split("_");
        final Locale inputLocale = new Locale(splitArgsInputLocale[0], splitArgsInputLocale[1]);
        final String[] splitArgsOutputLocale = args[1].split("_");
        final Locale outputLocale = new Locale(splitArgsOutputLocale[0], splitArgsOutputLocale[1]);

        if (OutputLocale.fromString(args[1]) == null) {
            System.err.println("Output locale is not supported");
            System.err.println("Supported location:");
            Arrays.stream(OutputLocale.values()).map(OutputLocale::getValue).sorted().forEach(System.err::println);
            return;
        }

        if (Arrays
                .stream(Locale.getAvailableLocales())
                .noneMatch(locale -> locale.getDisplayName().equals(inputLocale.getDisplayName()))) {
            System.err.println("Input locale is not supported");
            System.err.println("Supported location:");
            Arrays.stream(Locale.getAvailableLocales()).map(s -> String.format("tag = %s (%s)",s, s.getDisplayName())).sorted().forEach(System.err::println);
            return;
        }

        final Path inputPath = Paths.get(args[2]);
        final Path outputPath;
        try {
            final Path parent = Paths.get(args[3]).getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            outputPath = Paths.get(args[3]);
        } catch (IOException e) {
            System.err.println("Can not create output file");
            return;
        }

        try {
            new TextStatistics(Files.readString(inputPath), inputLocale, outputLocale).run(outputPath, inputPath.getFileName().toString());
        } catch (OutOfMemoryError e) {
            System.err.println("Input file is so big. Not enough memory.");
        } catch (SecurityException e) {
            System.err.println("Not enough permissions to read the file.");
        } catch (IOException e) {
            System.err.println("No such file");
        }
    }

    public TextStatistics(final String inputText, final Locale inputLocale, final Locale outputLocale) {
        this.inputText = inputText;
        this.resourceBundle = ResourceBundle.getBundle(STATISTICS_RESOURCE_BUNDLE, outputLocale);
        Locale.setDefault(outputLocale);
        Categories.setLocale(inputLocale);
        arrayCategories = new Categories[]{new Sentences(), new Words(), new Numbers(), new Money(), new Dates()};
    }

    public void run(final Path outputPath, final String inputFileName) {
        for (Categories category : arrayCategories) {
            final BreakIterator breakIterator = category.getBreakIterator();
            breakIterator.setText(inputText);
            int start = breakIterator.first();
            for (int end = breakIterator.next(); end != BreakIterator.DONE; start = end, end = breakIterator.next()) {
                final String string = inputText.substring(start, end).trim();
                if (category.isValid(string)) {
                    category.incrementOccurrencesNumber();
                    category.addUniqueValue(string);
                    category.setMinimumLength(string);
                    category.setMaximumLength(string);
                    category.setMinimumValue(string);
                    category.setMaximumValue(string);
                    category.addAverage(string);
                }
            }
        }

        try (final BufferedWriter writer = Files.newBufferedWriter(outputPath)) {
            write(writer, false, "AnalyzedFileKey", false, inputFileName);
            write(writer, false, "SummaryStatisticsKey", false);
            write(writer, true, "NumberSentencesKey", true,
                    arrayCategories[0].getOccurrencesNumber(), arrayCategories[0].getUniqueNumber());
            write(writer, true, "NumberWordsKey", true,
                    arrayCategories[1].getOccurrencesNumber(), arrayCategories[1].getUniqueNumber());
            write(writer, true, "NumberNumbersKey", true,
                    arrayCategories[2].getOccurrencesNumber(), arrayCategories[2].getUniqueNumber());
            write(writer, true, "NumberMoneyKey", true,
                    arrayCategories[3].getOccurrencesNumber(), arrayCategories[3].getUniqueNumber());
            write(writer, true, "NumberDatesKey", true,
                    arrayCategories[4].getOccurrencesNumber(), arrayCategories[4].getUniqueNumber());
            for (Categories category : Arrays.stream(arrayCategories)
                    .filter(s -> s.getOccurrencesNumber() != 0)
                    .collect(Collectors.toList())) {
                String categoryName = String.format("%s%s",
                        category.getClass().getSimpleName().substring(0, 1).toUpperCase(),
                        category.getClass().getSimpleName().substring(1).toLowerCase());
                writeCategory(writer, "Statistic%sKey", categoryName, false);
                writeCategory(writer, "Number%sKey", categoryName, true,
                        category.getOccurrencesNumber(), category.getUniqueNumber());
                writeCategory(writer, "MinValue%sKey", categoryName, false,
                        category.getValue(category.getMinimumValue()));
                writeCategory(writer, "MaxValue%sKey", categoryName, false,
                        category.getValue(category.getMaximumValue()));
                if (category instanceof Sentences || category instanceof Words) {
                    writeCategory(writer, "MinLength%sKey", categoryName, false,
                            category.getMinimumLength(), category.getMinimumLengthValue());
                    writeCategory(writer, "MinLength%sKey", categoryName, false,
                            category.getMaximumLength(), category.getMaximumLengthValue());
                }
                writeCategory(writer, "Average%sKey", categoryName, false, category.getAverage());
            }
        } catch (IOException | ParseException e) {
            e.printStackTrace();
        }
    }

    private void write(final BufferedWriter writer,
                       final boolean needTabulate,
                       final String key,
                       final boolean needChoice,
                       final Object... args) throws IOException {
        if (needTabulate) {
            writer.write("\t");
        }
        writer.write(resolveMessage(key, needChoice, args));
        writer.newLine();
    }

    private void writeCategory(final BufferedWriter writer,
                               final String patternKey,
                               final String categoryName,
                               final boolean needChoice,
                               final Object... args) throws IOException {
        if (!patternKey.equals("Statistic%sKey")) {
            writer.write("\t");
        }
        writer.write(resolveMessage(String.format(patternKey, categoryName), needChoice, args));
        writer.newLine();
    }

    private String resolveMessage(final String key,
                                  final boolean needChoice,
                                  final Object... args) {
        String pattern = resourceBundle.getString(key);
        if (needChoice) {
            ChoiceFormat choiceFormat = new ChoiceFormat(pattern);
            Format[] testFormats = {choiceFormat};
            MessageFormat messageFormat = new MessageFormat("{0}");
            messageFormat.setFormats(testFormats);
            return messageFormat.format(args);
        } else {
            return MessageFormat.format(pattern, args);
        }
    }

    public Categories[] getArrayCategories() {
        return arrayCategories;
    }
}
