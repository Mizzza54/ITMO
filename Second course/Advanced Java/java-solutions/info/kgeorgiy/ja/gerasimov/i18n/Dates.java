package info.kgeorgiy.ja.gerasimov.i18n;

import java.text.BreakIterator;
import java.text.DateFormat;
import java.text.ParseException;
import java.util.Comparator;
import java.util.Date;

/**
 * @author Michael Gerasimov
 * start: 05.06.2021
 * @version -
 */
public class Dates extends Categories {
    private final DateFormat dateFormat;
    private final Comparator<Date> comparator;
    private long averageValue = 0;

    public Dates() {
        dateFormat = DateFormat.getDateInstance(DateFormat.SHORT, getLocale());
        comparator = (e1, e2) -> (e1.before(e2) ? -1 : (e1.after(e2) ? 1 : 0));
    }

    @Override
    public BreakIterator getBreakIterator() {
        return BreakIterator.getLineInstance(getLocale());
    }

    @Override
    public boolean isValid(final String string) {
        try {
            dateFormat.parse(string);
            return true;
        } catch (ParseException e) {
            return false;
        }
    }

    @Override
    public int compare(final String value1, final String value2) {
        try {
            return comparator.compare(dateFormat.parse(value1), dateFormat.parse(value2));
        } catch (ParseException e) {
            e.printStackTrace();
        }
        return 0;
    }

    @Override
    public void addAverage(final String value) {
        try {
            averageValue += dateFormat.parse(value).getTime();
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }

    @Override
    public double getAverage() {
        return getOccurrencesNumber() != 0 ? (double) averageValue / getOccurrencesNumber() : 0;
    }

    @Override
    public Object getValue(final String value) throws ParseException {
        return dateFormat.parse(value);
    }
}
