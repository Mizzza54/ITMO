package expression.parser;

import expression.*;
import expression.exceptions.*;
import expression.generic.MyNumber;
import expression.operation.*;


/**
 * @author Michale Gerasimov
 * start: 10.12.2019
 * @version 17.12.2019
 */
public class ExpressionParser<T> implements GenericParser<T> {

    private Tokenizer tokenizer;
    private Token currentToken = Token.BEGIN;
    private MyNumber<T> parser;

    public ExpressionParser(MyNumber<T> parser) {
        this.parser = parser;
    }

    public TripleExpression<T> parse(final String expression) throws ParserMyException {
        tokenizer = new Tokenizer(expression, parser);
        currentToken = Token.BEGIN;
        TripleExpression<T> res = addAndSub();
        expect(Token.END);
        return res;
    }

    private void expect(Token token) throws ParserMyException {
        if (tokenizer.currentIndex <= tokenizer.expression.length() && token != currentToken) {
            if (currentToken == Token.END) {
                throw new ParserMyException("CLOSE_BRAKE_ERROR: MissingParenthesis at index " + tokenizer.currentIndex + " | Expression: " + tokenizer.getExpression(), tokenizer.currentIndex, tokenizer.expression.length());
            } else {
                throw new ParserMyException("ERROR: UnexpectedCharacter at index " + tokenizer.currentIndex + " | Expression: " + tokenizer.getExpression(), tokenizer.currentIndex, tokenizer.expression.length());
            }
        }
    }

    private TripleExpression<T> unary() throws ParserMyException {
        TripleExpression<T> res;
        currentToken = tokenizer.getNextToken();
        switch (currentToken) {
            case CONST:
                res = new Const(tokenizer.getLastValue());
                currentToken = tokenizer.getNextToken();
                break;
            case VARIABLE:
                res = new Variable(tokenizer.getLastVariable());
                currentToken = tokenizer.getNextToken();
                break;
            case NEGATIVE:
                res = new Negate(unary());
                break;
            case OPEN_BRAKE:
                res = addAndSub();
                currentToken = tokenizer.getNextToken();
                //  expect(Token.CLOSE_BRAKE);
                break;
            default:
                throw new ParserMyException("ERROR: UnexpectedCharacter at index " + tokenizer.currentIndex + " | Expression: " + tokenizer.getExpression(), tokenizer.currentIndex, tokenizer.expression.length());
        }
        return res;
    }

    private TripleExpression<T> mulAndDiv() throws ParserMyException {
        TripleExpression<T> res = unary();
        while (true) {
            switch (currentToken) {
                case MULTIPLY:
                    res = new Multiply(res, unary());
                    break;
                case DIVIDE:
                    res = new Divide(res, unary());
                    break;
                default:
                    return res;
            }
        }
    }

    private TripleExpression<T> addAndSub() throws ParserMyException {
        TripleExpression<T> res = mulAndDiv();
        while (true) {
            switch (currentToken) {
                case ADD:
                    res = new Add(res, mulAndDiv());
                    break;
                case SUBTRACT:
                    res = new Subtract(res, mulAndDiv());
                    break;
                default:
                    return res;
            }
        }
    }
}
