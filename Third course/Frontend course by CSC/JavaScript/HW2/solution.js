'use strict';

/**
 * Регулярное выражение задающее шаблон Email по стандарту RFC 5322.
 * @type {RegExp}
 */
const regExpEmail = /[^\s]+ /;

/**
 * Регулярное выражение задающее шаблон номера телефона.
 * @type {RegExp}
 */
const regExpPhoneNumber = /\d{10} /

/**
 * Регулярное выражение задающее шаблон имени в телефонной книге.
 * @type {RegExp}
 */
const regExpName = /([^;]*)/

/**
 * Функция, отвечающая является ли аргумент строкой.
 * @param {string} string - проверяемый аргумент.
 * @returns boolean - true если аргумент является строкой, иначе false.
 */
function isString(string) {
    return typeof string === "string";
}

/**
 * Функция, отвечающая является ли аргумент целым числом.
 * @param {string} string - проверяемый аргумент.
 * @returns boolean - true если аргумент является целым числом, иначе false.
 */
function isInteger(string) {
    return Number.isInteger(Number(string));
}

/**
 * Валидация имени контакта.
 * @param {string} name
 */
function isValidName(name) {
    if (!isString(name)) {
        throw new TypeError("Name should be string");
    }
    if (name.includes(';')) {
        // TO DO: Вернуть ошибку имени
    }
}

/**
 * Валидация запроса.
 * @param {string} request
 */
function isValidRequest(request) {
    if (!isString(request)) {
        throw new TypeError("Request should be string");
    }
    if (request.includes(';')) {
        // TO DO: Вернуть ошибку request
    }
}

/**
 * Валидация массива, содержащего информацию о контакте.
 * @param {string[]} array
 * @param {function} isFunction
 * @param {RegExp} pattern
 * @param {string} notArrayMessage
 * @param {string} invalidItemMessage
 * @param {string} patternErrorMessage
 */
function isValidInfoArray(array, isFunction, pattern, notArrayMessage, invalidItemMessage, patternErrorMessage) {
    if (!Array.isArray(array)) {
        throw new TypeError(notArrayMessage);
    }
    for (const item of array) {
        if (!isFunction(item)) {
            throw new TypeError(invalidItemMessage);
        }
        if (item.match(pattern) === null) {
            // TO DO: какие аргументы передать?
            // Ошибка номера - не совпал патерн
            syntaxError(0, 0);
        }
    }
}

/**
 * Данный класс содержит текстовое представление токена, его позицию в тексте и валидный ли он.
 */
class Token {
    /**
     *
     * @param text
     * @param {number} endPosition
     * @param {boolean} isValid
     * @param {number} countValid
     */
    constructor(text, endPosition, isValid, countValid) {
        this.results = text;
        this.endPosition = endPosition;
        this.isValid = isValid;
        this.countValid = countValid;
    }
}

/**
 * Функция для кобминирования разных типов парсеров.
 */
function ParserCombinator(parse) {
    this.parse = parse;
}

function textPattern(text) {
    return new ParserCombinator((str, pos) => {
        const substring = str.substr(pos, text.length);
        if (substring === text) {
            return new Token(substring, pos + str.substr(pos, text.length).length, true, 1);
        } else {
            return new Token(substring, pos + str.substr(pos, text.length).length, false, 0);
        }
    });
}

function regExpPattern(regExp) {
    return new ParserCombinator((str, pos) => {
        const match = str.slice(pos).match(regExp);
        if (match !== null && match.index === 0) {
            return new Token(match[0], pos + match[0].length, true, 1);
        } else {
            return new Token(str.slice(pos), pos + str.slice(pos).length, false, 0);
        }
    });
}

function anyPattern(...patterns) {
    return new ParserCombinator((str, pos) => {

        let countMaxValidPosition = -1;
        let resultMaxValid = 0;
        for (let i = 0; i < patterns.length; i++) {
            const result = patterns[i].parse(str, pos);
            if (result.isValid) {
                return result;
            }

            let max = 0;
            for (const item of result.results) {
                if (!item.isValid) {
                    break;
                }
                max = item.endPosition;
            }

            if (countMaxValidPosition < max) {
                countMaxValidPosition = max;
                resultMaxValid = result;
            }
            // if (result.results.length - result.countInvalid > countMaxValid) {
            //
            //     countMaxValid = result.results.length - result.countInvalid;
            //     resultMaxValid = result;
            // }
        }
        return resultMaxValid;
    });
}

function sequencePattern(...patterns) {
    return new ParserCombinator((str, pos) => {
        const results = [];
        let countInvalid = 0;
        let currentPosition = pos;
        for (const pattern of patterns) {
            const result = pattern.parse(str, currentPosition);

            if (Array.isArray(result.results)) {
                if (!result.isValid) {
                    countInvalid += result.countInvalid;
                }

                for (const item of result.results) {
                    results.push(item);
                }

                currentPosition = result.results[result.results.length - 1].endPosition;
            } else {
                if (!result.isValid) {
                    countInvalid++;
                }

                results.push(result);
                currentPosition = result.endPosition;
            }
        }
        return {results: results, isValid: countInvalid === 0, countInvalid: countInvalid}
    });
}

function repeatPattern(pattern, separator) {
    const separated = !separator ? pattern :
        sequencePattern(separator, pattern);

    return new ParserCombinator((str, pos) => {
        const results = [];
        let currentPosition = pos;
        let result = pattern.parse(str, currentPosition);
        if (!result.isValid) {
            return result;
        }

        while (result.isValid && result.results[result.results.length - 1].endPosition > currentPosition) {
            for (const item of result.results) {
                results.push(item);
            }
            currentPosition = results[results.length - 1].endPosition;
            result = separated.parse(str, currentPosition);
        }

        if (result.results[0].isValid && !result.results[result.results.length - 1].isValid) {
            for (const item of result.results) {
                results.push(item);
            }
        }

        let countInvalid = 0;
        for (const item of results) {
            if (!item.isValid) {
                countInvalid++;
            }
        }

        return {results: results, isValid: countInvalid === 0, countInvalid: countInvalid}
    });
}

function recursiveResultExtraction(argument) {

}

//let mtreg = /\s/
//const myPattern = sequencePattern(textPattern("Создай"), textPattern(" "));

const phonesParser1 = sequencePattern(textPattern("телефон "), regExpPattern(regExpPhoneNumber));
const emailsParser1 = sequencePattern(textPattern("почту "), regExpPattern(regExpEmail));
const namesParser2 = sequencePattern(textPattern("имя "))
const phonesParser2 = sequencePattern(textPattern("телефоны "))
const emailsParser2 = sequencePattern(textPattern("почты "))
const phonesAndEmailsParser1 = repeatPattern(anyPattern(phonesParser1, emailsParser1), textPattern("и "));
const phonesAndEmailsParser2 = repeatPattern(anyPattern(namesParser2, phonesParser2, emailsParser2), textPattern("и "));
const createContactParser = sequencePattern(textPattern("Создай "), textPattern("контакт "), regExpPattern(regExpName));
const deleteContactParser = sequencePattern(textPattern("Удали "), textPattern("контакт "), regExpPattern(regExpName));
const addInfoParser = sequencePattern(textPattern("Добавь "), phonesAndEmailsParser1, textPattern("для "), textPattern("контакта "), regExpPattern(regExpName));
const deleteInfoParser = sequencePattern(textPattern("Удали "), phonesAndEmailsParser1, textPattern("для "), textPattern("контакта "), regExpPattern(regExpName));
const findByRequestParser = sequencePattern(textPattern("Покажи "), phonesAndEmailsParser2, textPattern("для "), textPattern("контактов, "), textPattern("где "), textPattern("есть "), regExpPattern(regExpName));
const deleteByRequestParser = sequencePattern(textPattern("Удали "), textPattern("контакты, "), textPattern("где "), textPattern("есть "), regExpPattern(regExpName));


function parse(string, pos) {
    return anyPattern(
        createContactParser,
        deleteContactParser,
        addInfoParser,
        deleteInfoParser,
        findByRequestParser,
        deleteByRequestParser).parse(string, pos);
}



/**
 * Обработка одного запроса.
 * @param {string} query
 * @param lineNumber
 */
function processQuery(query, lineNumber) {
    const result = parse(query, 0);
    if (result.isValid) {
        switch (result.results[0].results) {
            case 'Создай ':
                createContact(result.results[2].results)
                break;
            case 'Добавь ':
                let phones = [];
                let emails = [];
                let state = 0;
                // 1 - phones, 2 - emails
                for (let i = 1; i < result.results.length - 3; i++) {
                    switch (result.results[i].results) {
                        case 'и ':
                            break;
                        case 'телефон ':
                            state = 1
                            break;
                        case 'почту ':
                            state = 2
                            break;
                        default:
                            if (state === 1) {
                                phones.push(result.results[i].results.slice(0, -1))
                            } else if (state === 2) {
                                emails.push(result.results[i].results.slice(0, -1))
                            }
                            break;
                    }
                }
                addInfo(phones, emails, result.results[result.results.length - 1].results)
                break;
            case 'Покажи ':
                const outputOrder = []
                for (let i = 1; i < result.results.length - 5; i++) {
                    switch (result.results[i].results) {
                        case 'и ':
                            break;
                        case 'имя ':
                            outputOrder.push('name');
                            break;
                        case 'телефоны ':
                            outputOrder.push('phone');
                            break;
                        case 'почты ':
                            outputOrder.push('email');
                            break;
                        default:
                            break;
                    }
                }
                return findByRequest(outputOrder, result.results[result.results.length - 1].results);
            default:
                switch (result.results[0].results + result.results[1].results) {
                    case 'Удали контакт ':
                        deleteContact(result.results[2].results)
                        break;
                    case 'Удали контакты, ':
                        deleteByRequest(result.results[4].results)
                        break;
                    default:
                        let phones = [];
                        let emails = [];
                        let state = 0;
                        // 1 - phones, 2 - emails
                        for (let i = 1; i < result.results.length - 3; i++) {
                            switch (result.results[i].results) {
                                case 'и ':
                                    break;
                                case 'телефон ':
                                    state = 1
                                    break;
                                case 'почту ':
                                    state = 2
                                    break;
                                default:
                                    if (state === 1) {
                                        phones.push(result.results[i].results.slice(0, -1))
                                    } else if (state === 2) {
                                        emails.push(result.results[i].results.slice(0, -1))
                                    }
                                    break;
                            }
                        }
                        deleteInfo(phones, emails, result.results[result.results.length - 1].results)
                        break;
                }
                break;
        }
    } else {
        let index = 0;
        let item = result.results[index];
        while (item.isValid) {
            index++;
            item = result.results[index];
        }
        //console.log(item)
        syntaxError(lineNumber, item.endPosition - item.results.length + 1)
    }
}

//
//processQuery('Создай контакт Миша');
//processQuery('Удали контакт Миша');
//processQuery("Добавь телефон 5556667788 и телефон 5556667787 и почту grisha@example.com для контакта Григорий");
//processQuery('Удали телефон 5556667788 для контакта Григорий')
//processQuery('Покажи имя и почты и телефоyны и почты для контактов, где есть Гр')
//processQuery('Покажи  имя для контактов, где есть Гр')
//Добавь телефон 5556667788 и телефон 5556667787 для контакта Григорий
let order = 0;

/**
 * Функция, которая выполняет команду "Создай контакт <имя>"
 * @param {string} name
 */
function createContact(name) {
    isValidName(name);
    // if (phoneBook.has(name)) {
    //     // TO DO: Вывести в err что контакт уже есть
    //     return;
    // }

    if (phoneBook.get(name) === undefined) {
        order++;
        phoneBook.set(name, {
            phones: new Set(),
            emails: new Set(),
            order: order
        })
    }
}

/**
 * Функция, которая выполняет команду "Удали контакт <имя>"
 * @param {string} name
 */
function deleteContact(name) {
    phoneBook.delete(name);
}

/**
 * Функция, которая выполняет команду "Добавь телефон <телефон> и почту <почта> для контакта <имя>"
 * @param {string[]} phones
 * @param {string[]} emails
 * @param {string} name
 */
function addInfo(phones, emails, name) {
    // isValidName(name);
    // isValidInfoArray(
    //     phones,
    //     isInteger,
    //     regExpPhoneNumber,
    //     "Phones should be array of numbers",
    //     "Elements of phones array should be integer numbers",
    //     "TO DO"
    // )
    // isValidInfoArray(
    //     emails,
    //     isString,
    //     regExpEmail,
    //     "Phones should be array of strings",
    //     "Elements of emails array should be strings",
    //     "TO DO"
    // )

    if (phoneBook.get(name) !== undefined) {
        phones.forEach(item => phoneBook.get(name).phones.add(item));
        emails.forEach(item => phoneBook.get(name).emails.add(item));
    }
}

/**
 * Функция, которая выполняет команду "Удали телефон <телефон> и почту <почта> для контакта <имя>"
 * @param {string[]} phones
 * @param {string[]} emails
 * @param {string} name
 */
function deleteInfo(phones, emails, name) {
    // isValidName(name);
    // isValidInfoArray(
    //     phones,
    //     isInteger,
    //     regExpPhoneNumber,
    //     "Phones should be array of numbers",
    //     "Elements of phones array should be integer numbers",
    //     "TO DO"
    // )
    // isValidInfoArray(
    //     emails,
    //     isString,
    //     regExpEmail,
    //     "Phones should be array of strings",
    //     "Elements of emails array should be strings",
    //     "TO DO"
    // )

    if (phoneBook.get(name) !== undefined) {
    phones.forEach(item => {phoneBook.get(name).phones.delete(item)});
    emails.forEach(item => phoneBook.get(name).emails.delete(item));
    }
}

/**
 * Функция, которая выполняет команду "Покажи почты и телефоны для контактов, где есть <запрос>"
 * @param outputOrder
 * @param {string} request
 */
function findByRequest(outputOrder, request) {
    if (request === '') {
        return
    }
    const result = [];
    phoneBook.forEach((value, key) => {
        if (key.includes(request) || Array.from(value.phones).some(elem => elem.includes(request)) || Array.from(value.emails).some(elem => elem.includes(request))) {
            result.push({name: key, phones: value.phones, emails: value.emails, order: value.order});
        }
    })
    result.sort((a, b) => a.order - b.order);
    const answer = [];
    answer.push();
    for (const resultElement of result) {
        const tempArray = []
        for (const item of outputOrder) {
            switch (item) {
                case 'name':
                    tempArray.push(resultElement.name);
                    break;
                case 'phone':
                    tempArray.push(Array.from(resultElement.phones).map(elem => {
                        const triple1 = elem.substring(0, 3)
                        const triple2 = elem.substring(3, 6)
                        const duo1 = elem.substring(6, 8)
                        const duo2 = elem.substring(8, 10)
                        return `+7 (${triple1}) ${triple2}-${duo1}-${duo2}`
                    }).join(','))
                    break;
                case 'email':
                    tempArray.push(Array.from(resultElement.emails).join(','))
                    //answer[index].concat(resultElement.emails);
                    break
            }
        }
        answer.push(tempArray.join(';'))
    }
    return answer;
}

/**
 * Функция, которая выполняет команду "Удали контакты, где есть <запрос>"
 * @param {string} request
 */
function deleteByRequest(request) {
    if (request === '') {
        return
    }
    const keys = [];
    phoneBook.forEach((value, key) => {
        if (key.includes(request) || Array.from(value.phones).some(elem => elem.includes(request)) || Array.from(value.emails).some(elem => elem.includes(request))) {
            keys.push(key);
        }
    })
    for (const key of keys) {
        phoneBook.delete(key);
    }
}

/**
 * Телефонная книга
 */
const phoneBook = new Map();

/**
 * Вызывайте эту функцию, если есть синтаксическая ошибка в запросе
 * @param {number} lineNumber – номер строки с ошибкой
 * @param {number} charNumber – номер символа, с которого запрос стал ошибочным
 */
function syntaxError(lineNumber, charNumber) {
    throw new Error(`SyntaxError: Unexpected token at ${lineNumber}:${charNumber}`);
}

/**
 * Выполнение запроса на языке pbQL
 * @param {string} query
 * @returns {string[]} - строки с результатами запроса
 */
function run(query) {
    if (!isString(query)) {
        throw new TypeError("Query should be string");
    }
    let result = [];
    let lastString;

    let flag = false;
    if (query[query.length - 1] !== ';') {
        flag = true;
    }

    const splitQuery = query.split(';');
    if (query[query.length - 1] !== ';') {
        lastString = splitQuery[splitQuery.length - 1]
    }
    if (splitQuery[splitQuery.length - 1] === '') {
        splitQuery.pop()
    }

    for (let i = 0; i < splitQuery.length; i++) {
        const temp = processQuery(splitQuery[i], i + 1)
        if (Array.isArray(temp)) {
            for (const tempElement of temp) {
                result.push(tempElement)
            }
        }
        //result.push();
    }

    if (flag) {
        syntaxError(splitQuery.length, lastString.length + 1);
    }

    result = result.filter(function( element ) {
        return element !== undefined;
    });
    return result;
}

//module.exports = { phoneBook, run };
module.exports = { phoneBook, run, };

// console.log(run('Создай контакт Миша;Добавь телефон 5556667788 и телефон 5556667787 и почту grisha@example.com для контакта Миша;'))
// console.log(phoneBook)
// console.log(run('Покажи имя и почты и телефоны и почты для контактов, где есть Миша;'))
// console.log(run('Удали контакт Миша;'))
// console.log(phoneBook)
// console.log(run('Покажи имя и почты и телефоны и почты для контактов, где есть Миша;'))
// console.log(run('Создай контакт Миша1;Добавь телефон 5556667788 и телефон 5556667787 и почту grisha@example.com для контакта Миша1;'))
// console.log(run('Создай контакт Миша2;Добавь телефон 5556667788 и телефон 5556667787 и почту grisha@example.com для контакта Миша2;'))
// console.log(run('Создай контакт Миша3;Добавь телефон 5556667788 и телефон 5556667787 и почту grisha@example.com для контакта Миша3;'))
// console.log(phoneBook)
// console.log(run('Покажи имя и почты и телефоны и почты для контактов, где есть Миша;'))
// console.log(run('Покажи имя и почты и телефоны и почты для контактов, где есть Миша1;'))
// console.log(run('Удали телефон 5556667788 для контакта МишаNot;'))
// console.log(phoneBook)
// console.log(run('Удали телефон 5556667788 для контакта Миша1;'))
// console.log(phoneBook)
// console.log(run('Покажи имя и почты и телефоны и почты для контактов, где есть Миша;'))
// console.log(run('Удали контакты, где есть Миша;'))
// console.log(run('Покажи имя и почты и телефоны и почты для контактов, где есть Миша;'))
// console.log(run(
//     'Создай контакт Григорий;' +
//     'Удали контакт Григорий;' +
//     'Покажи имя для контактов, где есть ий;'
// ))
// console.log(run(
//     'Создай контакт Григорий;' +
//     'Добавь телефон 5556667787 для контакта Григорий;' +
//     'Добавь телефон 5556667788 и почту grisha@example.com для контакта Григорий;' +
//     'Покажи имя и телефоны и почты для контактов, где есть ий'
// ))
// console.log(run('Создай контакт Миша;' +
//     'Создай контакт Сергей;' +
//     'Добавь телефон 9067855005 для контакта Миша;'+
//     'Добавь телефон 9039039020 для контакта Сергей;' +
//     'Покажи имя для контактов, где есть 9;'))
// console.log(phoneBook)

// console.log(run('Создай контакт Миша;' +
//     'Создай контакт Сергей;' +
//     'Добавь телефон 90678550051 для контакта Миша;'+
//     'Добавь телефон 90390390201 для контакта Сергей;' +
//     'Покажи имя для контактов, где есть 9;'))

// console.log(run(
//     'Создай контакт Григорий;' +
//     'Создай контакт Василий;' +
//     'Создай контакт Иннокентий;' +
//     'Покажи имя и имя и имя для контактов, где есть ий;'
// ))
// console.log(phoneBook)

// console.log(run(
//     'Создай контакт Григорий;' +
//     'Добавь телефон 5556667787 для контакта Григорий;' +
//     'Добавь телефон 5556667788 и почту grisha@example.com для контакта Григорий;' +
//     'Покажи имя и телефоны и почты для контактов, где есть ий;'
// ))


//
// console.log(run('Создай контакт Миша;' +
//     'Создай контакт Сергей;' +
//     'Добавь телефон 9067855005 для контакта Миша;'+
//     'Добавь телефон 9039039020 для контакта Сергей;' +
//     'Покажи имя и телефоны для контактов, где есть 9;' +
//     'Удали контакты, где есть  ;' +
//     'Покажи имя и телефоны для контактов, где есть 9;'))

// console.log(run('Создай контакт  ;' +
//     'Создай контакт Сергей;' +
//     'Добавь телефон 9067855005 для контакта  ;'+
//     'Добавь телефон 9039039020 для контакта Сергей;' +
//     'Покажи имя и телефоны и почты для контактов, где есть 9;' +
//     'Удали контакты, где есть ;' +
//     'Покажи имя и телефоны для контактов, где есть 9;'))