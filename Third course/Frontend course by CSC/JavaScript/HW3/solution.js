'use strict';

const MIN_PER_HOUR = 60;
const MIN_PER_DAY = 24 * MIN_PER_HOUR
const DAYS_OF_WEEK = new Map([
    ["ПН", 0],
    ["ВТ", MIN_PER_DAY],
    ["СР", MIN_PER_DAY * 2],
    ["ЧТ", MIN_PER_DAY * 3],
    ["ПТ", MIN_PER_DAY * 4],
    ["СБ", MIN_PER_DAY * 5],
    ["ВС", MIN_PER_DAY * 6],
]);

let bankTimeZone, bankOpeningTime, bankClosingTime;

function parseTime(time) {
    const split = time.split(/[ :+]/);
    const day = DAYS_OF_WEEK.get(split[0]);
    const hour = +split[1];
    const minute = +split[2];
    const timeZone = +split[3];
    return day +
        hour * MIN_PER_HOUR +
        minute +
        (bankTimeZone - timeZone) * MIN_PER_HOUR;
}


function processingBankTime(workingHours) {
    bankTimeZone = workingHours.from.split("+")[1]
    const splitFrom = workingHours.from.substr(0, 5).split(":")
    const splitTo = workingHours.to.substr(0, 5).split(":")
    bankOpeningTime = +splitFrom[0] * MIN_PER_HOUR + +splitFrom[1]
    bankClosingTime = +splitTo[0] * MIN_PER_HOUR + +splitTo[1]
}

function processingGangTime(schedule) {
    const result = {};

    for (let name of Object.keys(schedule)) {
        if (!(name in result)) {
            result[name] = []
        }
        for (let item of schedule[name]) {
            result[name].push({from: parseTime(item.from), to: parseTime(item.to)})
        }
        result[name].sort((a, b) => a.from - b.to)
    }

    return result
}

function findFreeTimes(gangBusyTimes) {
    const result = {};
    let start = parseTime(`ПН 00:00+${bankTimeZone}`)

    for (let name of Object.keys(gangBusyTimes)) {
        if (!(name in result)) {
            result[name] = []
        }
        for (let item of gangBusyTimes[name]) {
            result[name].push({from: start, to: item.from})
            start = item.to
        }
        result[name].push({from: start, to: parseTime(`СР 23:59+${bankTimeZone}`)})
        start = parseTime(`ПН 00:00+${bankTimeZone}`)
    }
    return result
}

function createBankTimes() {
    return [
        {from: bankOpeningTime, to: bankClosingTime},
        {from: bankOpeningTime + MIN_PER_DAY, to: bankClosingTime + MIN_PER_DAY},
        {from: bankOpeningTime + MIN_PER_DAY * 2, to: bankClosingTime + MIN_PER_DAY * 2}
    ];
}

function findIntersectionTimeIntervals(gangFreeTimes) {
    const gangIntersection = []
    let i = 0, j = 0, k = 0;
    const danny = gangFreeTimes["Danny"]
    const rusty = gangFreeTimes["Rusty"]
    const linus = gangFreeTimes["Linus"]
    const bank = createBankTimes()
    while (i < danny.length && j < rusty.length && k < linus.length) {
        let low = Math.max(danny[i].from, rusty[j].from, linus[k].from)
        let high = Math.min(danny[i].to, rusty[j].to, linus[k].to)
        if (low <= high) {
            gangIntersection.push({from: low, to: high})
        }

        switch (high) {
            case danny[i].to:
                i++
                break
            case rusty[j].to:
                j++
                break
            case linus[k].to:
                k++
                break
        }
    }

    const result = []
    for (let item1 of gangIntersection) {
        if (item1 === undefined) {
            continue
        }

        for (let item2 of bank) {
            let temp = isIntersection(item1.from, item1.to, item2.from, item2.to)
            if (temp !== undefined) {
                result.push(temp)
            }
        }
    }

    return result;
}

function isIntersection (l1, r1, l2, r2) {
    if (r2 >= l1 && r1 >= l2) {
        return {from: Math.max(l1, l2), to: Math.min(r1, r2)}
    } else {
        return undefined;
    }
}

function findPossibleTime (timeline, duration) {
    return timeline.filter(x => x.to - x.from >= duration)
}

/**
 * @param {Object} schedule Расписание Банды
 * @param {number} duration Время на ограбление в минутах
 * @param {Object} workingHours Время работы банка
 * @param {string} workingHours.from Время открытия, например, "10:00+0"
 * @param {string} workingHours.to Время закрытия, например, "18:00+0"
 * @returns {Object}
 */
function getAppropriateMoment(schedule, duration, workingHours) {
    let possibleHeistTime
    let currentChoice = 0
    let durationShift = 0
    init()

    function init() {
        processingBankTime(workingHours)
        // console.info("---------------------------------------------")
        // console.info("Bank time zone = " + bankTimeZone)
        // console.info("Bank open at " + bankOpeningTime + " minutes")
        // console.info("Bank close at " + bankClosingTime + " minutes")
        // console.info("---------------------------------------------\n")

        const gangBusyTimes = processingGangTime(schedule)
        // console.info("---------------------------------------------")
        // console.info("This is time, when members of gang are busy:")
        // console.info(gangBusyTimes)
        // console.info("---------------------------------------------\n")

        const gangFreeTimes = findFreeTimes(gangBusyTimes)
        // console.info("---------------------------------------------")
        // console.info("This is time, when members of gang are FREE!:")
        // console.info(gangFreeTimes)
        // console.info("---------------------------------------------\n")

        const intersectionTime = findIntersectionTimeIntervals(gangFreeTimes)
        // console.info("---------------------------------------------")
        // console.info("This is time, when free time of gang intersection with time of bank:")
        // console.info(intersectionTime)
        // console.info("---------------------------------------------\n")

        possibleHeistTime = findPossibleTime(intersectionTime, duration)
        // console.info("---------------------------------------------")
        // console.info("This is time, when gang can start Heist!:")
        // console.info(possibleHeistTime)
        // console.info("---------------------------------------------\n")
    }


    return {
        /**
         * Найдено ли время
         * @returns {boolean}
         */
        exists() {
            return possibleHeistTime.length > 0
        },

        /**
         * Возвращает отформатированную строку с часами
         * для ограбления во временной зоне банка
         *
         * @param {string} template
         * @returns {string}
         *
         * @example
         * ```js
         * getAppropriateMoment(...).format('Начинаем в %HH:%MM (%DD)') // => Начинаем в 14:59 (СР)
         * ```
         */
        format(template) {
            if (!this.exists()) {
                return "";
            }

            let time = possibleHeistTime[currentChoice].from + durationShift

            let dd;
            switch (Math.floor(time / MIN_PER_DAY)) {
                case 0:
                    dd = "ПН";
                    break;
                case 1:
                    dd = "ВТ";
                    break;
                case 2:
                    dd = "СР";
                    break;
            }

            let hh = Math.floor(time / 60) % 24
            if (hh < 10) {
                hh = '0' + hh
            }

            let mm = time % 60
            if (mm < 10) {
                mm = '0' + mm
            }

            return template.replace('%DD', dd)
                .replace('%HH', '' + hh)
                .replace('%MM', '' + mm)
        },

        /**
         * Попробовать найти часы для ограбления позже [*]
         * @note Не забудь при реализации выставить флаг `isExtraTaskSolved`
         * @returns {boolean}
         */
        tryLater() {
            if (currentChoice >= possibleHeistTime.length) {
                return false
            }

            if (possibleHeistTime[currentChoice].to - (possibleHeistTime[currentChoice].from + durationShift) >= duration + 30) {
                durationShift += 30
                return true;
            } else if (currentChoice < possibleHeistTime.length - 1) {
                durationShift = 0
                currentChoice++
                return true
            } else {
                return false
            }
        }
    };
}


const isExtraTaskSolved = true

module.exports = {
    getAppropriateMoment,
    isExtraTaskSolved
};
