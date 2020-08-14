package expression.parser;

import expression.*;
import expression.exceptions.*;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface GenericParser<T> {
    TripleExpression<T> parse(String expression) throws ParserMyException;
}