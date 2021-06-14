package info.kgeorgiy.ja.gerasimov.i18n;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.BreakIterator;
import java.util.Arrays;
import java.util.Locale;
import java.util.function.Consumer;

import static org.junit.Assert.assertEquals;

/**
 * @author Michael Gerasimov
 * start: 05.06.2021
 * @version -
 */
public class TextStatisticsTest {
    private Sentences sentences;
    private Words words;
    private Numbers numbers;
    private Money money;
    private Dates dates;
    protected String testMethodName;

    private static final Path TESTS_DIR = Path.of("tests");
    private static final Path OUT = Path.of("./tempOutput.txt");

    @Rule
    public TestRule watcher = watcher(description -> {
        testMethodName = description.getMethodName();
        System.err.println("=== Running " + testMethodName);
    });

    protected static TestWatcher watcher(final Consumer<Description> watcher) {
        return new TestWatcher() {
            @Override
            protected void starting(final Description description) {
                watcher.accept(description);
            }
        };
    }

    public void initCategories(TextStatistics textStatistics) {
        Categories[] categories = textStatistics.getArrayCategories();
        sentences = (Sentences) categories[0];
        words = (Words) categories[1];
        numbers = (Numbers) categories[2];
        money = (Money) categories[3];
        dates = (Dates) categories[4];
    }

    public Path getControlPath() {
        return TESTS_DIR.resolve(testMethodName + ".txt");
    }

    public void checkOccurrencesNumber(int expectedSentences,
                                       int expectedWords,
                                       int expectedNumbers,
                                       int expectedMoney,
                                       int expectedDates) {
        assertEquals(sentences.getOccurrencesNumber(), expectedSentences);
        assertEquals(words.getOccurrencesNumber(), expectedWords);
        assertEquals(numbers.getOccurrencesNumber(), expectedNumbers);
        assertEquals(money.getOccurrencesNumber(), expectedMoney);
        assertEquals(dates.getOccurrencesNumber(), expectedDates);
    }

    public int findOccurrencesNumberWordsUS(String text) {
        String[] splitText = text.split("\\s+");
        return (int) Arrays.stream(splitText).filter(s -> words.isValid(s)).count();
    }

    public void checkAverage(double expectedSentences,
                             double expectedWords,
                             double expectedNumbers,
                             double expectedMoney,
                             double expectedDates) {
        assertEquals(sentences.getAverage(), expectedSentences, 0.00001);
        assertEquals(words.getAverage(), expectedWords, 0.00001);
        assertEquals(numbers.getAverage(), expectedNumbers, 0.00001);
        assertEquals(money.getAverage(), expectedMoney, 0.00001);
        assertEquals(dates.getAverage(), expectedDates, 0.00001);
    }

    public double findAverageSentencesUS(String text) {
        String[] splitText = text.split("\\.\\s");
        int sum = 0;
        for (String str: splitText) {
            sum += str.trim().length() + 1;
            //System.out.println(str + ".length = " + (str.trim().length() + 1));
        }
        //System.out.println(splitText.length);
        return (double) sum / splitText.length;
    }

    public double findAverageWordsUS(String text, Locale locale) {
        BreakIterator breakIterator = BreakIterator.getWordInstance(locale);
        breakIterator.setText(text);
        int start = breakIterator.first();
        int sum = 0;
        int count = 0;
        for (int end = breakIterator.next(); end != BreakIterator.DONE; start = end, end = breakIterator.next()) {
            String string = text.substring(start, end).trim();
            if (string.codePoints().anyMatch(Character::isLetter)) {
                count++;
                sum += string.length();
            }
        }
        return (double) sum / count;
    }

    public void checkControlFile(Path controlPath, Path testingPath) {
        try {
            assertEquals(Files.readString(controlPath), Files.readString(testingPath));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @Test
    public void testEmptyText() {
        TextStatistics textStatistics = new TextStatistics("", Locale.US, new Locale("ru", "RU"));
        textStatistics.run(OUT, getControlPath().getFileName().toString());
        initCategories(textStatistics);
        checkOccurrencesNumber(0,0,0,0,0);
        checkControlFile(getControlPath(), OUT);
    }

    @Test
    public void testEnUSFileWithOnlyWords() {
        String text = "Java is C++ without the guns, clubs and knives - James Gosling.\n" + // length = 63
                      "If Java had true garbage collection, most programs would delete themselves upon execution - Robert Sewell.\n" + // length = 106
                      "Java development without a little heresy would be a dull place, and a dangerous one - Bruce Tate.\n" + // length = 97
                      "I'm sorry but you lose - Georgiy Korneev. "; // length = 41
        TextStatistics textStatistics = new TextStatistics(text, Locale.US, new Locale("ru", "RU"));
        textStatistics.run(OUT, getControlPath().getFileName().toString());
        initCategories(textStatistics);
        checkOccurrencesNumber(4,50,0,0,0);
        checkAverage(findAverageSentencesUS(text), findAverageWordsUS(text, Locale.US),0.0,0.0,0.0);
        checkControlFile(getControlPath(), OUT);
    }

    @Test
    public void testEnUSFile() {
        String text = "Alan Mathison Turing was born 06/01/1912 and die 06/07/1954.\n" +
                      "He was an English mathematician, computer scientist, logician, cryptanalyst, philosopher, and theoretical biologist.\n" +
                      "Turing was highly influential in the development of theoretical computer science, providing a formalisation of the concepts of algorithm and computation with the Turing machine, which can be considered a model of a general-purpose computer.\n" +
                      "Turing is widely considered to be the father of theoretical computer science and artificial intelligence.\n" +
                      "Born in Maida Vale, London, Turing was raised in southern England.\n" +
                      "He graduated at King's College, Cambridge with a degree in mathematics.\n" +
                      "Whilst he was a fellow at Cambridge, he published a proof demonstrating that some purely mathematical yes–no questions can never be answered by computation and defined a Turing machine, and went on to prove the halting problem for Turing machines is undecidable.\n" +
                      "In 1938, he obtained his PhD from the Department of Mathematics at Princeton University.\n" +
                      "During the Second World War, Turing worked for the Government Code and Cypher School at Bletchley Park, Britain's codebreaking centre that produced Ultra intelligence.\n" +
                      "For a time he led Hut 8, the section that was responsible for German naval cryptanalysis.\n" +
                      "Here, he devised a number of techniques for speeding the breaking of German ciphers, including improvements to the pre-war Polish bombe method, an electromechanical machine that could find settings for the Enigma machine.\n" +
                      "Turing played a crucial role in cracking intercepted coded messages that enabled the Allies to defeat the Nazis in many crucial engagements, including the Battle of the Atlantic.\n" +
                      "Due to the problems of counterfactual history, it is hard to estimate the precise effect Ultra intelligence had on the war.\n" +
                      "However, Professor Jack Copeland has estimated that this work shortened the war in Europe by more than two years and saved over 14 million lives. ";
        TextStatistics textStatistics = new TextStatistics(text, Locale.US, new Locale("ru", "RU"));
        textStatistics.run(OUT, getControlPath().getFileName().toString());
        initCategories(textStatistics);
        checkOccurrencesNumber(14, findOccurrencesNumberWordsUS(text),5,0,2);
        checkAverage(findAverageSentencesUS(text), findAverageWordsUS(text, Locale.US),(double) (6 + 6 + 1938 + 8 + 14) / 5,0.0, -1.1543139085E12);
        checkControlFile(getControlPath(), OUT);
    }

    @Test
    public void testEnUSMoney() {
        String text =
                "Kidneys - $200,000.\n" +
                "Kidneys are by far the most popular organ on the Black Market.\n" +
                "Liver - $157,000.\n" +
                "Liver disease, including alcoholic liver disease, hepatitis B and C, cirrhosis, and carcinoma, is on the rise in the US, leading more people to require transplants.\n" +
                "As of 2015, approximately 30,000 Americans die each year from chronic liver diseases.\n" +
                "Waiting for a transplant often takes too long, and the cost of this surgery is also very prohibitive for many patients.\n" +
                "Turning to the Black Market makes it cheaper and easier to find a replacement liver.\n" +
                "If you were to sell your liver today, it would bring in about $157,000.\n" +
                "Heart - $119,000.\n" +
                "If you're like the Tin Man from The Wizard of Oz and you need a new heart to stay alive, you may be very tempted to turn to the Black Market.\n" +
                "Legal heart donations require $997,700 dollars, which is a major chunk of the estimated $1.4 million fee associated with heart transplant surgery.\n" +
                "Corneas - $30,000.\n" +
                "Cornea damage can lead to blindness, so it's no wonder that this body part is valuable.\n" +
                "After all, people can permanently damage their eyesight from a long list of seemingly innocuous things, including glancing at a solar eclipse without proper eye protection.\n" +
                "Coronary Artery - $1,525.\n" +
                "Heart disease is one of the most common ailments a person can develop.\n" +
                "There are numerous surgeries utilized to help this issue, ranging from a coronary bypass to a heart transplant.\n" +
                "Blood - $337 Per Pint.\n" +
                "Even though most people are eligible to donate blood, there's still typically a big shortage of this necessary life-giving component of the human body, both for individual patients and pharmaceutical companies.\n" +
                "Therefore, illegal blood farming has become a surprisingly common practice.\n" +
                "In India, some people become trapped in so-called blood farms where they're kept in cages and forced to donate blood. ";
        TextStatistics textStatistics = new TextStatistics(text, Locale.US, new Locale("ru", "RU"));
        textStatistics.run(OUT, getControlPath().getFileName().toString());
        initCategories(textStatistics);
        checkOccurrencesNumber(21, findOccurrencesNumberWordsUS(text),2,9,0);
        double money = (200_000 + 157_000 + 157_000 + 119_000 + + 997_700 + 1.4 + 30_000 + 1_525 + 337) / 9;
        checkAverage(findAverageSentencesUS(text), findAverageWordsUS(text, Locale.US),(double) (30000 + 2015) / 2, money, 0.0);
        checkControlFile(getControlPath(), OUT);
    }

    @Test
    public void testChineseWords() {
        String text = "該文本是中文的特殊文本，出於說明目的，是辛勤工作的一部分。 這是第二句話。 這是第三句話。";
        TextStatistics textStatistics = new TextStatistics(text, Locale.CHINESE, new Locale("en", "US"));
        textStatistics.run(OUT, getControlPath().getFileName().toString());
        initCategories(textStatistics);
        checkOccurrencesNumber(3, 5,0,0,0);
    }

    @Test
    public void testBengaliWords() {
        String text = "এই পাঠ্যটি বাংলায় একটি বিশেষ পাঠ, বিক্ষোভমূলক উদ্দেশ্যে যা কঠোর পরিশ্রমের অংশ হিসাবে অর্পণ করা হয়েছে। এটি দ্বিতীয় বাক্য। এবং এটি তৃতীয় বাক্য।";
        TextStatistics textStatistics = new TextStatistics(text, new Locale("bn", "IN"), new Locale("en", "US"));
        textStatistics.run(OUT, getControlPath().getFileName().toString());
        initCategories(textStatistics);
        checkOccurrencesNumber(3, 23,0,0,0);
    }
}