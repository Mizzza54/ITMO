"use strict";

function operation(core, operator) {
    function Constructor() {
        const tmp = [];
        tmp.push.apply(tmp, arguments);
        this.args = tmp;
    }

    Constructor.prototype.toString = function () {
        let str = [];
        for (let key of this.args) {
            str += key + " ";
        }
        str += operator;
        return str
    };

    Constructor.prototype.evaluate = function () {
        return core.apply(null, this.args.map(getValue(arguments)))
    };

    Constructor.prototype.prefix = function () {
        return "(" + operator + " " + this.args.map(getPrefix).join(" ") + ")"
    };

    return Constructor;
}

function getPrefix(expression) {
    return expression.prefix();
}

function getValue(args) {
    return function (expression) {
        return expression.evaluate.apply(expression, args);
    }
}


function Const(cnst) {
    this.cnst = cnst
}
Const.prototype.toString = function () {return this.cnst.toString()};
Const.prototype.evaluate = function () {return this.cnst};
Const.prototype.prefix = function () {
    return this.cnst.toString();
};


let vars = {'x': 0, 'y': 1, 'z': 2};
function Variable(name) {this.name = name}
Variable.prototype.toString = function () {return this.name.toString()};
Variable.prototype.evaluate = function () {
    let id;
    id = vars[this.name];
    return arguments[id];
};
Variable.prototype.prefix = function () {
    return this.name;
};


const Add = operation(function (x, y) {return x + y}, "+");
const Subtract = operation(function (x, y) {return x - y}, "-");
const Multiply = operation(function (x, y) {return x * y}, "*");
const Divide = operation(function (x, y) {return x / y}, "/");
const Negate = operation(function (x) {return -x}, "negate");
const ArcTan = operation(function (x) {return Math.atan(x)}, "atan");
const Exp = operation(function (x) {return Math.exp(x)}, "exp");


let map = {
    '+': 2,
    '-': 2,
    '/': 2,
    '*': 2,
    'negate': 1,
    'exp': 1,
    'atan': 1
};

let mapObj = {
    '+': Add,
    '-': Subtract,
    '/': Divide,
    '*': Multiply,
    'negate': Negate,
    'exp': Exp,
    'atan': ArcTan
};

function ParserError(message) {
    this.name = "ParserError";
    this.message = message;
}
ParserError.prototype = Error.prototype;

//console.log(parsePrefix("(/ 2 (+ 3 x))").prefix());

function parsePrefix(expression) {
    let Tokens = [];
    let stack = [];
    let balance = 0;
    let index = 0;
    let tmpR;
    let tmpL;
    parseToken();
    if (Tokens.length === 0) {
        throw new ParserError("Empty input");
    }
    index = 0;
    let expr = Parser();
    while (index < Tokens.length && Tokens[index] === ')') {
        balance--;
        index++;
    }
    if (balance !== 0 || index < Tokens.length) {
        throw new ParserError("Bad end " + expression);
    }
    return expr;

    function parseToken() {
        while (index < expression.length) {
            while (expression[index] === " ") {
                index++;
            }
            switch (expression[index]) {
                case '(':
                    Tokens.push('(');
                    break;
                case ')':
                    Tokens.push(')');
                    break;
                default:
                    let start = index;
                    while (index < expression.length && expression[index] !== '(' && expression[index] !== ')' && expression[index] !== ' ') {
                        index++;
                    }
                    if (expression.substring(start, index) !== '') {
                        Tokens.push(expression.substring(start, index));
                    }
                    index--;
                    break;
            }

            index++;
        }
    }

    function Parser() {
        if (index < Tokens.length) {
            switch (Tokens[index]) {
                case '(':
                    balance++;
                    index++;
                    if (map[Tokens[index]] === undefined) {
                        throw new ParserError("Invalid operation at index " + index + " expression = " + expression);
                    }
                    return Parser();
                case ')':
                    balance--;
                    break;
                default:
                    if (map[Tokens[index]] !== undefined) {
                        return parseOperate();
                    } else if (!isNaN(parseInt(Tokens[index]))) {
                        index++;
                        return new Const(parseInt(Tokens[index - 1]));
                    } else if (vars[Tokens[index]] !== undefined) {
                        index++;
                        return new Variable(Tokens[index - 1]);
                    } else {
                        throw new ParserError("Unknown variable " + Tokens[index]);
                    }
            }
        }
    }

    function parseOperate() {
        tmpL = [];
        let tmp = Tokens[index];
        index++;
        for (let i = 0; i < map[tmp]; i++) {
            stack.push(parseOperand());
        }


        for (let i = 0; i < map[tmp]; i++) {
            tmpL.push(stack.pop());
        }
        tmpL.reverse();
        return new mapObj[tmp](...tmpL.slice(0, map[tmp]));
    }

    function parseOperand() {
        if (Tokens[index] === '(' || map[Tokens[index]] !== undefined) {
            return Parser();
        } else if (Tokens[index] === ')') {
            index++;
            balance--;
            return parseOperand();
        } else if (vars[Tokens[index]] !== undefined) {
            index++;
            return new Variable(Tokens[index - 1]);
        } else if (!isNaN(parseInt(Tokens[index]))) {
            index++;
            return new Const(getNumber(Tokens[index - 1]));
        } else {
            throw new ParserError('Invalid operation at index ' + index  + ' expression = ' + expression);
        }
    }

    function getNumber(s) {
        let numbers = "0123456789";
        let l = s.length;
        for (let i = s[0] === "-" ? 1 : 0; i < l; i++) {
            if (numbers.indexOf(s[i]) === -1)
                throw new ParserError("Invalid symbol in argument: " + s.substring(0, i) + "[->" + s[i] + "<-]" + s.substring(i + 1, l) + " at index " + index);
        }
        let result = parseInt(s);
        if (isNaN(result))
            throw new ParserError("Invalid number: " + s + " at index " + index);
        return result;
    }
}
