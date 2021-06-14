package info.kgeorgiy.ja.gerasimov.i18n;

import java.text.BreakIterator;
import java.text.Collator;
import java.text.ParseException;
import java.util.Comparator;

/**
 * @author Michael Gerasimov
 * start: 05.06.2021
 * @version -
 */
public class Words extends Categories {
    private final Comparator<Object> comparator;
    private int averageValue = 0;

    public Words() {
        comparator = Collator.getInstance(getLocale());
    }

    @Override
    public BreakIterator getBreakIterator() {
        return BreakIterator.getWordInstance();
    }

    @Override
    public boolean isValid(final String string) {
        return string.codePoints().anyMatch(Character::isLetter);
    }

    @Override
    public int compare(final String value1, final String value2) {
        return comparator.compare(value1, value2);
    }

    @Override
    public void addAverage(final String value) {
        averageValue += value.length();
    }

    @Override
    public double getAverage() {
        return getOccurrencesNumber() != 0 ? (double) averageValue / getOccurrencesNumber() : 0;
    }

    @Override
    public Object getValue(final String value) throws ParseException {
        return value;
    }
}
