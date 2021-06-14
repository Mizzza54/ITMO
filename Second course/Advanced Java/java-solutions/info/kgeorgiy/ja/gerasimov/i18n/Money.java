package info.kgeorgiy.ja.gerasimov.i18n;

import java.text.BreakIterator;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.Comparator;

/**
 * @author Michael Gerasimov
 * start: 05.06.2021
 * @version -
 */
public class Money extends Categories {
    private final NumberFormat currencyFormat;
    private final Comparator<Number> comparator;
    private double averageValue = 0;

    public Money() {
        currencyFormat = NumberFormat.getCurrencyInstance(getLocale());
        comparator = (e1, e2) ->
                ((int) Math.signum(e1.doubleValue() - e2.doubleValue()));
    }

    @Override
    public BreakIterator getBreakIterator() {
        return BreakIterator.getWordInstance(getLocale());
    }

    @Override
    public boolean isValid(final String string) {
        try {
            currencyFormat.parse(string);
            return true;
        } catch (ParseException e) {
            return false;
        }
    }

    @Override
    public int compare(final String value1, final String value2) {
        try {
            return comparator.compare(currencyFormat.parse(value1), currencyFormat.parse(value2));
        } catch (ParseException e) {
            e.printStackTrace();
        }
        return 0;
    }

    @Override
    public void addAverage(final String value) {
        try {
            averageValue += currencyFormat.parse(value).doubleValue();
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }

    @Override
    public double getAverage() {
        return getOccurrencesNumber() != 0 ? averageValue / getOccurrencesNumber() : 0.0;
    }

    @Override
    public Object getValue(final String value) throws ParseException {
        return currencyFormat.parse(value);
    }
}
