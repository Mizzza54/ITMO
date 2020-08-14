"use strict";

const consts = ["x", "y", "z"];

const binaryOperation = function (op) {
    return function (left, right) {
        return function (x, y, z) {
            return op(left(x, y, z), right(x, y, z))
        }
    }
};

const unaryOperation = function (op) {
    return function (expression) {
        return function (x, y, z) {
            return op(expression(x, y, z))
        }
    }
};

const cnst = value => x => value;

const variable = (name) => (...values) => {
    let id;
    switch (name) {
        case "x": id = 0; break;
        case "y": id = 1; break;
        case "z": id = 2; break;
    }
    return values[id];
};

const add = binaryOperation(function (left, right) {
    return left + right;
});

const divide = binaryOperation(function (left, right) {
    return left / right;
});

const subtract = binaryOperation(function (left, right) {
    return left - right;
});

const multiply = binaryOperation(function (left, right) {
    return left * right;
});

const negate = unaryOperation(function (expression) {
    return -expression;
});

const cube = unaryOperation(function (expression) {
    return expression * expression * expression
});

const cuberoot = unaryOperation( function (expression) {
    return Math.cbrt(expression)
});

