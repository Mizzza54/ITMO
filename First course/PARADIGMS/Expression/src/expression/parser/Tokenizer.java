package expression.parser;

import expression.exceptions.ParserMyException;

/**
 * @author Michale Gerasimov
 * start: 15.12.2019
 * @version 18.12.2019
 */
public class Tokenizer {
    private char[] bufChars;
    public int currentIndex;
    public int lastValue;
    public String newConst;
    public Token currentToken;
    public String name;
    public String expression;


    public Tokenizer(String string) {
        bufChars = string.toCharArray();
        expression = string;
        currentIndex = 0;
        currentToken = Token.BEGIN;
    }

    public String getExpression() {
        return expression;
    }

    public int getLastValue() {
        return lastValue;
    }

    public String getLastVariable() {
        return name;
    }

    public void skipWhiteSpace() {
        while (currentIndex < expression.length() && Character.isWhitespace(expression.charAt(currentIndex))) {
            currentIndex++;
        }
    }

    public Token getCurrentToken() {
        return currentToken;
    }

    public Token getNextToken() throws ParserMyException {
        nextToken();
        return currentToken;
    }

    public void nextToken() throws ParserMyException {
        skipWhiteSpace();
        if (currentIndex >= expression.length()) {
            if (currentToken != Token.CONST && currentToken != Token.CLOSE_BRAKE && currentToken != Token.VARIABLE) {
                currentToken = Token.END;
                throw new ParserMyException("WRONG_END_ERROR | Expression: " + getExpression(), currentIndex, expression.length());
            } else {
                currentToken = Token.END;
                return;
            }
        }

        char c = bufChars[currentIndex];
        if ((Character.isDigit(c))) {
            NumberToken(c);
        } else if (Character.isLetter((c))) {
            WordToken(c);
        } else {
            SymbolToken(c);
        }
        currentIndex++;
    }

    private void NumberToken(char c) throws ArithmeticException{
        StringBuilder sb = new StringBuilder();
        while (currentIndex < bufChars.length && Character.isDigit(bufChars[currentIndex])) {
            sb.append(bufChars[currentIndex]);
            currentIndex++;
        }
        currentIndex--;
        try {
            lastValue = Integer.parseUnsignedInt(sb.toString());
            if (currentToken == Token.NEGATIVE) {
                Integer.parseInt("-" + sb.toString());
            } else {
                Integer.parseInt(sb.toString());
            }
            newConst = sb.toString();
            currentToken = Token.CONST;
        } catch (NumberFormatException e) {
            throw new ArithmeticException("Const is overflow");
        }
    }

    private void WordToken(char c) throws ParserMyException{
        if (c == 'x' || c == 'y' || c == 'z') {
            currentToken = Token.VARIABLE;
            name = String.valueOf(c);
        } else if (c == 'p' || c == 'l') {
            SymbolToken(c);
        } else {
            throw new ParserMyException("WRONG_NAME_ERROR: Variable has wrong name | Expression: " + getExpression(), currentIndex, expression.length());
        }

    }

    private void SymbolToken(char c) throws ParserMyException {
        switch (c) {
            case '+':
                if (currentToken == Token.CONST || currentToken == Token.CLOSE_BRAKE || currentToken == Token.VARIABLE) {
                    currentToken = Token.ADD;
                    break;
                } else {
                    throw new ParserMyException("ADD_ERROR: Cant find argument at index " + currentIndex + " | Expression: " + getExpression(), currentIndex, expression.length());
                }
            case '-':
                if (currentToken == Token.CONST || currentToken == Token.VARIABLE || currentToken == Token.CLOSE_BRAKE) {
                    currentToken = Token.SUBTRACT;
                } else  {
                    //expected
                    currentToken = Token.NEGATIVE;
                }
                break;
            case '/':
                if (currentToken == Token.CONST || currentToken == Token.CLOSE_BRAKE || currentToken == Token.VARIABLE) {
                    currentToken = Token.DIVIDE;
                    break;
                } else {
                    throw new ParserMyException("DIVIDE_ERROR: Cant find argument at index " + currentIndex + " | Expression: " + getExpression(), currentIndex, expression.length());
                }
            case '*':
                if (currentToken == Token.CONST || currentToken == Token.CLOSE_BRAKE || currentToken == Token.VARIABLE) {
                    currentToken = Token.MULTIPLY;
                    break;
                } else {
                    throw new ParserMyException("MULTIPLY_ERROR: Cant find argument at index " + currentIndex + " | Expression: " + getExpression(), currentIndex, expression.length());
                }
            case '(':
                if (currentToken == Token.ADD || currentToken == Token.DIVIDE || currentToken == Token.MULTIPLY || currentToken == Token.BEGIN || currentToken == Token.SUBTRACT
                        || currentToken == Token.NEGATIVE || currentToken == Token.OPEN_BRAKE || currentToken == Token.POW2 || currentToken == Token.LOG2) {
                    //expected
                    currentToken = Token.OPEN_BRAKE;
                    break;
                } else {
                    throw new ParserMyException("OPEN_BRAKE_ERROR: Wrong Parenthesis at index " + currentIndex + " | Expression: " + getExpression(), currentIndex, expression.length());
                }
            case ')':
                if (currentToken == Token.CONST || currentToken == Token.VARIABLE || currentToken == Token.CLOSE_BRAKE) {
                    currentToken = Token.CLOSE_BRAKE;
                    break;
                } else {
                    throw new ParserMyException("CLOSE_BRAKE_ERROR: Wrong Parenthesis at index " + currentIndex + " | Expression: " + getExpression(), currentIndex, expression.length());
                }
            case 'l':
                expect("og2");
                currentToken = Token.LOG2;
                break;
            case 'p':
                expect("ow2");
                currentToken = Token.POW2;
                break;
            default:

                /*
                if (currentIndex + 2 <= expression.length() && expression.substring(currentIndex, currentIndex + 2).equals("<<")) {
                    currentIndex += 2;
                    currentToken = Token.SHIFT_LEFT;
                } else if (currentIndex + 2 <= expression.length() && expression.substring(currentIndex, currentIndex + 2).equals(">>")) {
                    currentIndex += 2;
                    currentToken = Token.SHIFT_RIGHT;
                }
                 */
                //System.err.println(">>" + c + "<<");
                throw new ParserMyException("ERROR: UnexpectedCharacter at index " + currentIndex + " | Expression: " + getExpression(), currentIndex, expression.length());
                //break;
        }
    }

    private void expect(String s) throws ParserMyException {
        StringBuilder sb = new StringBuilder();
        int i = 3;
        currentIndex++;
        while (i != 0) {
            sb.append(bufChars[currentIndex]);
            currentIndex++;
            i--;
        }
        currentIndex--;
        if (!s.equals(sb.toString())) {
            throw new ParserMyException("WRONG_OPERATION_ERROR: Unary operation has mistake| Expression: " + getExpression(), currentIndex, expression.length());
        }
    }
}