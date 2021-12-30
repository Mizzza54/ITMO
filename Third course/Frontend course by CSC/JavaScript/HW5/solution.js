'use strict';

const bfs = function (phonebook) {
    const queue = [];
    const layers = [];
    layers[0] = [];
    const visited = new Set();

    const map = new Map();
    phonebook.forEach(f => map.set(f.name, f))


    const bestPhonebook = phonebook.filter(x => x.best);
    bestPhonebook.forEach(x => {
        x.level = 0;
        visited.add(x);
        queue.push(x);
        layers[0].push(x);
    });

    while (queue.length > 0) {
        const node = queue.shift();

        if (!layers[node.level + 1]) {
            layers[node.level + 1] = []
        }

        for (let i = 0; i < node.friends.length; i++) {
            const friend = map.get(node.friends[i]);

            if (!visited.has(friend)) {
                visited.add(friend);
                layers[node.level + 1].push(friend);
                friend.level = node.level + 1;
                queue.push(friend);
            }
        }
    }

    return layers;
}

/**
 * Итератор по друзьям
 * @constructor
 * @param {Object[]} friends
 * @param {Filter} filter
 */
function Iterator(friends, filter, maxLevel = Infinity) {

    const compare = (a, b) => {
        if (a.name < b.name) {
            return -1
        }

        if (a.name > b.name) {
            return 1
        }

        return 0
    }

    const layers = bfs(friends);
    let iterator = [];
    layers
        .forEach(level =>
            level
                .filter(x => filter.filter(x.gender) && x.level < maxLevel)
                .sort(compare)
                .forEach(x => iterator.push(x)));

    this.next = function () {
        if (this.done()) {
            return null;
        }
        return iterator.shift();
    }

    this.done = function () {
        return iterator.length === 0;
    }
}

/**
 * Итератор по друзям с ограничением по кругу
 * @extends Iterator
 * @constructor
 * @param {Object[]} friends
 * @param {Filter} filter
 * @param {Number} maxLevel – максимальный круг друзей
 */
function LimitedIterator(friends, filter, maxLevel) {
    Iterator.call(this, friends, filter, maxLevel);
}

LimitedIterator.prototype = Object.create(Iterator.prototype)
LimitedIterator.prototype.constructor = LimitedIterator

/**
 * Фильтр друзей
 * @constructor
 */
function Filter() {
    this.filter = function (gender) {
        return true;
    }
}

/**
 * Фильтр друзей
 * @extends Filter
 * @constructor
 */
function MaleFilter() {
    this.filter = function (gender) {
        return gender === 'male'
    }
}
MaleFilter.prototype = Object.create(Filter.prototype);
MaleFilter.prototype.constructor = MaleFilter

/**
 * Фильтр друзей-девушек
 * @extends Filter
 * @constructor
 */
function FemaleFilter() {
    this.filter = function (gender) {
        return gender === 'female'
    }
}
FemaleFilter.prototype = Object.create(Filter.prototype);
FemaleFilter.prototype.constructor = FemaleFilter

exports.Iterator = Iterator;
exports.LimitedIterator = LimitedIterator;

exports.Filter = Filter;
exports.MaleFilter = MaleFilter;
exports.FemaleFilter = FemaleFilter;