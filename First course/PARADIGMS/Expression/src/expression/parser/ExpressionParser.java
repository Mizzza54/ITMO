package expression.parser;

import expression.*;
import expression.exceptions.*;
import operation.*;
import operation.check.*;


/**
 * @author Michale Gerasimov
 * start: 10.12.2019
 * @version 17.12.2019
 */
public class ExpressionParser implements Parser{

    private Tokenizer tokenizer;
    private Token currentToken = Token.BEGIN;

    public int getCurrentIndex() {
        return tokenizer.currentIndex;
    }

    public CommonExpression parse(final String expression) throws ParserMyException {
        tokenizer = new Tokenizer(expression);
        currentToken = Token.BEGIN;
        CommonExpression res = addAndSub();
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

    private CommonExpression unary() throws ParserMyException {
        CommonExpression res;
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
                res = new CheckedNegate(unary());
                break;
            case OPEN_BRAKE:
                res = addAndSub();
                currentToken = tokenizer.getNextToken();
                //  expect(Token.CLOSE_BRAKE);
                break;
            case POW2:
                res = new CheckedPow2(unary());
                break;
            case LOG2:
                res = new CheckedLog2(unary());
                break;
            default:
                throw new ParserMyException("ERROR: UnexpectedCharacter at index " + tokenizer.currentIndex + " | Expression: " + tokenizer.getExpression(), tokenizer.currentIndex, tokenizer.expression.length());
        }
        return res;
    }

    private CommonExpression mulAndDiv() throws ParserMyException {
        CommonExpression res = unary();
        while (true) {
            switch (currentToken) {
                case MULTIPLY:
                    res = new CheckedMultiply(res, unary());
                    break;
                case DIVIDE:
                    res = new CheckedDivide(res, unary());
                    break;
                default:
                    return res;
            }
        }
    }

    private CommonExpression addAndSub() throws ParserMyException {
        CommonExpression res = mulAndDiv();
        while (true) {
            switch (currentToken) {
                case ADD:
                    res = new CheckedAdd(res, mulAndDiv());
                    break;
                case SUBTRACT:
                    res = new CheckedSubtract(res, mulAndDiv());
                    break;
                default:
                    return res;
            }
        }
    }

    private CommonExpression shifts() throws ParserMyException {
        CommonExpression res = addAndSub();
        while (true) {
            if (tokenizer.getCurrentToken() == Token.SHIFT_LEFT) {
                res = new ShiftLeft(res, addAndSub());
            } else if (tokenizer.getCurrentToken() == Token.SHIFT_RIGHT) {
                res = new ShiftRight(res, addAndSub());
            } else {
                return res;
            }
        }
    }
}
