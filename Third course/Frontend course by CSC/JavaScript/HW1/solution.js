'use strict';

/**
 * Складывает два целых числа
 * @param {Number} a Первое целое
 * @param {Number} b Второе целое
 * @throws {TypeError} Когда в аргументы переданы не числа
 * @returns {Number} Сумма аргументов
 */
function abProblem(a, b) {
    if (typeof a !== "number" || typeof b !== "number") {
        throw new TypeError("Arguments are not numbers");
    }
    return a + b;
}

/**
 * Определяет век по году
 * @param {Number} year Год, целое положительное число
 * @throws {TypeError} Когда в качестве года передано не число
 * @throws {RangeError} Когда год – отрицательное значение
 * @returns {Number} Век, полученный из года
 */
function centuryByYearProblem(year) {
    if (typeof year !== "number") {
        throw new TypeError("Argument is not number");
    }

    if (year < 0) {
        throw new RangeError("Argument can not be negative");
    }

    return Math.floor(year / 100) + ((year % 10 > 0) ? 1 : 0);
}

/**
 * Переводит цвет из формата HEX в формат RGB
 * @param {String} hexColor Цвет в формате HEX, например, '#FFFFFF'
 * @throws {TypeError} Когда цвет передан не строкой
 * @throws {RangeError} Когда значения цвета выходят за пределы допустимых
 * @returns {String} Цвет в формате RGB, например, '(255, 255, 255)'
 */
function colorsProblem(hexColor) {
    if (typeof hexColor !== "string") {
        throw new TypeError("Argument must be a String");
    }

    const pattern = /^#\p{Hex_Digit}{1,6}$/u;
    const match = hexColor.match(pattern);

    if (match === null) {
        throw new RangeError("A value of color out of the range");
    }

    let temp = "";
    if (hexColor.length !== 7) {
        temp += ("0".repeat(7 - hexColor.length));
    }
    temp += (hexColor.substr(1, hexColor.length - 1));
    const rgb = [parseInt(temp[0] + temp[1], 16),
                 parseInt(temp[2] + temp[3], 16),
                 parseInt(temp[4] + temp[5], 16)];
    return "(" + rgb.join(", ") + ")";
}

/**
 * Находит n-ое число Фибоначчи
 * @param {Number} n Положение числа в ряде Фибоначчи
 * @throws {TypeError} Когда в качестве положения в ряде передано не число
 * @throws {RangeError} Когда положение в ряде не является целым положительным числом
 * @returns {Number} Число Фибоначчи, находящееся на n-ой позиции
 */
function fibonacciProblem(n) {
    if (typeof n !== "number") {
        throw new TypeError("Argument must be a Number");
    }

    if (!Number.isInteger(n) || n <= 0) {
        throw new RangeError("Argument is not natural number");
    }

    let a = 1;
    let b = 1;
    for (let i = 3; i <= n; i++) {
        const c = a + b;
        a = b;
        b = c;
    }
    return b;
}

/**
 * Транспонирует матрицу
 * @param {(Any[])[]} matrix Матрица размерности MxN
 * @throws {TypeError} Когда в функцию передаётся не двумерный массив
 * @returns {(Any[])[]} Транспонированная матрица размера NxM
 */
function matrixProblem(matrix) {

    if (!Array.isArray(matrix) || !Array.isArray(matrix[0])) {
        throw new TypeError("Bad matrix");
    }

    const rowLength = matrix[0].length;
    const result = [];

    for (let i = 0; i < rowLength; i++) {
        result.push([]);
    }

    for (let i = 0; i < matrix.length; i++) {

        if (matrix[i].length !== rowLength) {
            throw new TypeError("Bad matrix");
        }

        for (let j = 0; j < rowLength; j++) {
            if (Array.isArray(matrix[i][j])) {
                throw new TypeError("Bad matrix");
            }
            result[j][i] = matrix[i][j];
        }
    }

    return result;
}

/**
 * Переводит число в другую систему счисления
 * @param {Number} n Число для перевода в другую систему счисления
 * @param {Number} targetNs Система счисления, в которую нужно перевести (Число от 2 до 36)
 * @throws {TypeError} Когда переданы аргументы некорректного типа
 * @throws {RangeError} Когда система счисления выходит за пределы значений [2, 36]
 * @returns {String} Число n в системе счисления targetNs
 */
function numberSystemProblem(n, targetNs) {
    if (typeof n !== "number" || typeof targetNs !== "number") {
        throw new TypeError("Arguments must be a Number");
    }

    if (targetNs < 2 || targetNs > 36) {
        throw new RangeError("targetNs must be from 2 to 36");
    }

    return n.toString(targetNs);
}

/**
 * Проверяет соответствие телефонного номера формату
 * @param {String} phoneNumber Номер телефона в формате '8–800–xxx–xx–xx'
 * @throws {TypeError} Когда в качестве аргумента передаётся не строка
 * @returns {Boolean} Если соответствует формату, то true, а иначе false
 */
function phoneProblem(phoneNumber) {
    if (typeof phoneNumber !== "string") {
        throw new TypeError("Argument must be a String");
    }

    const pattern = /^8-800-\d{3}-\d{2}-\d{2}$/;
    return phoneNumber.match(pattern) !== null;
}

/**
 * Определяет количество улыбающихся смайликов в строке
 * @param {String} text Строка в которой производится поиск
 * @throws {TypeError} Когда в качестве аргумента передаётся не строка
 * @returns {Number} Количество улыбающихся смайликов в строке
 */
function smilesProblem(text) {
    if (typeof text !== "string") {
        throw new TypeError("Argument must be a String");
    }

    const pattern = /(:-\))|(\(-:)/g;
    const match = Array.from(text.matchAll(pattern));
    if (match[0] == null) {
        return 0;
    } else {
        let result = 0;
        for (const i of match) {
            result++;
        }
        return result;
    }
}

/**
 * Определяет победителя в игре "Крестики-нолики"
 * Тестами гарантируются корректные аргументы.
 * @param {(('x' | 'o')[])[]} field Игровое поле 3x3 завершённой игры
 * @returns {'x' | 'o' | 'draw'} Результат игры
 */
function ticTacToeProblem(field) {
    for (let i = 0; i < 3; i++) {
        if (field[i][0] === field[i][1] && field[i][1] === field[i][2]) {
            return field[i][0];
        }

        if (field[0][i] === field[1][i] && field[1][i] === field[2][i]) {
            return field[0][i];
        }
    }

    if (field[0][0] === field[1][1] && field[1][1] === field[2][2]) {
        return field[0][0];
    }

    if (field[0][2] === field[1][1] && field[1][1] === field[2][0]) {
        return field[1][1];
    }

    return 'draw';
}

module.exports = {
    abProblem,
    centuryByYearProblem,
    colorsProblem,
    fibonacciProblem,
    matrixProblem,
    numberSystemProblem,
    phoneProblem,
    smilesProblem,
    ticTacToeProblem
};