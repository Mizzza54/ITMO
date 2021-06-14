package info.kgeorgiy.ja.gerasimov.i18n;

import java.text.BreakIterator;
import java.text.ParseException;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

/**
 * @author Michael Gerasimov
 * start: 05.06.2021
 * @version -
 */
public abstract class Categories {
    private static Locale locale;
    private int occurrencesNumber = 0;
    private String minimumValue = null;
    private String maximumValue = null;
    private int minimumLength = -1;
    private int maximumLength = -1;
    private String minimumLengthValue = null;
    private String maximumLengthValue = null;
    private final Set<String> visitedValues = new HashSet<>();

    public int getOccurrencesNumber() {
        return occurrencesNumber;
    }

    public void setOccurrencesNumber(int occurrencesNumber) {
        this.occurrencesNumber = occurrencesNumber;
    }

    public void incrementOccurrencesNumber() {
        occurrencesNumber++;
    }

    public String getMinimumValue() {
        return minimumValue.trim();
    }

    public void setMinimumValue(String minimumValue) {
        if (this.minimumValue == null) {
            this.minimumValue = minimumValue;
        } else {
            this.minimumValue = compare(this.minimumValue, minimumValue) < 0 ? this.minimumValue : minimumValue;
        }
    }

    public String getMaximumValue() {
        return maximumValue.trim();
    }

    public void setMaximumValue(final String maximumValue) {
        if (this.maximumValue == null) {
            this.maximumValue = maximumValue;
        } else {
            this.maximumValue = compare(this.maximumValue, maximumValue) > 0 ? this.maximumValue : maximumValue;
        }
    }

    public int getMinimumLength() {
        return minimumLength;
    }

    public void setMinimumLength(final String minimumLengthValue) {
        if (this.minimumLength == -1) {
            this.minimumLength = minimumLengthValue.length();
            this.minimumLengthValue = minimumLengthValue;
        } else {
            if (this.minimumLength > minimumLengthValue.length()) {
                this.minimumLength = minimumLengthValue.length();
                this.minimumLengthValue = minimumLengthValue;
            }
        }
    }

    public int getMaximumLength() {
        return maximumLength;
    }

    public void setMaximumLength(final String maximumLengthValue) {
        if (this.maximumLength == -1) {
            this.maximumLength = maximumLengthValue.length();
            this.maximumLengthValue = maximumLengthValue;
        } else {
            if (this.maximumLength < maximumLengthValue.length()) {
                this.maximumLength = maximumLengthValue.length();
                this.maximumLengthValue = maximumLengthValue;
            }
        }
    }

    public Locale getLocale() {
        return locale;
    }

    public static void setLocale(final Locale locale) {
        Categories.locale = locale;
    }

    public String getMinimumLengthValue() {
        return minimumLengthValue.trim();
    }

    public String getMaximumLengthValue() {
        return maximumLengthValue.trim();
    }

    public void addUniqueValue(final String value) {
        visitedValues.add(value);
    }

    public int getUniqueNumber() {
        return visitedValues.size();
    }

    abstract public BreakIterator getBreakIterator();

    abstract public boolean isValid(final String string);

    abstract public int compare(final String value1, final String value2);

    abstract public void addAverage(final String value);

    abstract public double getAverage();

    abstract public Object getValue(final String value) throws ParseException;
}
