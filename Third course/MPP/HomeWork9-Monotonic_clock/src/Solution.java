import org.jetbrains.annotations.NotNull;

/**
 * В теле класса решения разрешено использовать только финальные переменные типа RegularInt.
 * Нельзя volatile, нельзя другие типы, нельзя блокировки, нельзя лазить в глобальные переменные.
 *
 * @author Michael Gerasimov
 */
public class Solution implements MonotonicClock {
    private final RegularInt c1 = new RegularInt(0);
    private final RegularInt c2 = new RegularInt(0);
    private final RegularInt c3 = new RegularInt(0);

    private final RegularInt v1 = new RegularInt(0);
    private final RegularInt v2 = new RegularInt(0);
    @Override
    public void write(@NotNull Time time) {
        // write right-to-left
        c1.setValue(time.getD1());
        c2.setValue(time.getD2());
        c3.setValue(time.getD3());

        v2.setValue(time.getD2());
        v1.setValue(time.getD1());
    }

    @NotNull
    @Override
    public Time read() {
        int vv1 = v1.getValue();
        int vv2 = v2.getValue();

        int cc3 = c3.getValue();
        int cc2 = c2.getValue();
        int cc1 = c1.getValue();

        int r1 = cc1;
        int r2 = cc1 == vv1 ? cc2 : 0;
        int r3 = cc1 == vv1 && cc2 == vv2 ? cc3 : 0;

        return new Time(r1, r2, r3);
    }
}
