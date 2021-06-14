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
public class Numbers extends Categories {
    private final NumberFormat numberFormat;
    private final Comparator<Double> comparator;
    private double averageValue = 0;

    public Numbers() {
        numberFormat = NumberFormat.getNumberInstance(getLocale());
        comparator = (e1, e2) ->
                ((int) Math.signum(e1 - e2));
    }

    @Override
    public BreakIterator getBreakIterator() {
        return BreakIterator.getLineInstance(getLocale());
    }

    @Override
    public boolean isValid(final String string) {
        try {
            numberFormat.parse(string);
            return true;
        } catch (ParseException e) {
            return false;
        }
    }

    @Override
    public int compare(final String value1, final String value2) {
        try {
            return comparator.compare(numberFormat.parse(value1).doubleValue(), numberFormat.parse(value2).doubleValue());
        } catch (ParseException e) {
            e.printStackTrace();
        }
        return 0;
    }

    @Override
    public void addAverage(final String value) {
        try {
            averageValue += numberFormat.parse(value).doubleValue();
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }

    @Override
    public double getAverage() {
        return getOccurrencesNumber() != 0 ? averageValue / getOccurrencesNumber() : 0;
    }

    @Override
    public Object getValue(final String value) throws ParseException {
        return numberFormat.parse(value);
    }
}
