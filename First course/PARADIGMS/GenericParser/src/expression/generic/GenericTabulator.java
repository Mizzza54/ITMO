package expression.generic;

import expression.TripleExpression;
import expression.generic.numbers.*;
import expression.parser.ExpressionParser;

import java.util.Map;

public class GenericTabulator implements Tabulator {
    private Map<String, MyNumber<?>> MODES = Map.of(
            "i", new IntegerNumbers(),
            "d", new DoubleNumbers(),
            "bi", new BigIntegerNumbers(),
            "l", new LongNumbers(),
            "s", new ShortNumbers()
    );

    public Object[][][] tabulate(final String mode, final String str, final int x1, final int x2, final int y1,
                                 final int y2, final int z1, final int z2) throws Exception {
        MyNumber<?> numbers = MODES.get(mode);
        return tabulate(numbers, str, x1, x2, y1, y2, z1, z2);
    }

    private <T> Object[][][] tabulate(final MyNumber<T> op, final String str, final int x1, final int x2,
                                      final int y1, final int y2, final int z1, final int z2) throws Exception {
        final TripleExpression<T> expression = new ExpressionParser<T>(op).parse(str);

        int dx = x2 - x1 + 1;
        int dy = y2 - y1 + 1;
        int dz = z2 - z1 + 1;
        Object[][][] result = new Object[dx][dy][dz];
        for (int x = 0; x < dx; x++) {
            for (int y = 0; y < dy; y++) {
                for (int z = 0; z < dz; z++) {
                    try {
                        result[x][y][z] = expression.evaluate(op.parseToDigitTab(Integer.toString(x + x1)), op.parseToDigitTab(Integer.toString(y + y1)),
                                op.parseToDigitTab(Integer.toString(z + z1)), op);
                    } catch (ArithmeticException ignored) {
                    }
                }
            }
        }
        return result;
    }
}