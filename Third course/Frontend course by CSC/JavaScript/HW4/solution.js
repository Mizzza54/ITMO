/**
 * Возвращает новый emitter
 * @returns {Object}
 */
function getEmitter() {
    let map = new Map();

    return {
        /**
         * Подписаться на событие
         * @param {String} event
         * @param {Object} context
         * @param {Function} handler
         * @param {Number} severalCount
         * @param {Number} throughCount
         */
        on: function (event, context, handler, severalCount = null, throughCount = null) {
            if (!map.has(event)) {
                map.set(event, [])
            }

            map.get(event).push({
                context: context,
                handler: handler,
                severalCount: severalCount,
                throughCount: throughCount,
                count: 0
            })

            return this;
        },

        /**
         * Отписаться от события
         * @param {String} event
         * @param {Object} context
         */
        off: function (event, context) {
            Array.from(map.keys()).forEach(k => {
                if (k === event || k.startsWith(event + '.')) {
                    map.set(k, map.get(k).filter(i => i.context !== context));
                }
            })
            return this;
        },

        /**
         * Уведомить о событии
         * @param {String} event
         */
        emit: function (event) {
            const events = parseEvent(event);
            events
                .filter(k => map.has(k))
                .forEach(k => {
                    const array = map.get(k);

                    for (let i = 0; i < array.length; i++) {
                        const item = array[i];
                        const context = item.context;
                        const handler = item.handler;
                        const severalCount = item.severalCount;
                        const throughCount = item.throughCount;
                        const count = item.count;

                        if (severalCount == null && throughCount == null) {
                            handler.apply(context);
                        } else if (throughCount == null) {
                            if (severalCount > 0) {
                                handler.apply(context);
                                //severalCount--;
                            }
                        } else if (severalCount == null) {
                            if (count === 0 || count % throughCount === 0) {
                                handler.apply(context);
                            }
                        }

                        array[i] = {
                            context: context,
                            handler: handler,
                            severalCount: severalCount === null ? null : severalCount - 1,
                            throughCount: throughCount,
                            count: count + 1
                        }
                        //count++;
                    }
                })
            return this;
        },

        /**
         * Подписаться на событие с ограничением по количеству полученных уведомлений
         * @star
         * @param {String} event
         * @param {Object} context
         * @param {Function} handler
         * @param {Number} times – сколько раз получить уведомление
         */
        several: function (event, context, handler, times) {
            this.on(event, context, handler, times);
            return this;
        },

        /**
         * Подписаться на событие с ограничением по частоте получения уведомлений
         * @star
         * @param {String} event
         * @param {Object} context
         * @param {Function} handler
         * @param {Number} frequency – как часто уведомлять
         */
        through: function (event, context, handler, frequency) {
            this.on(event, context, handler, null, frequency);
            return this;
        },

        getMap: function () {
            return map;
        }
    };
}

function parseEvent(string) {
    const splitted = string.split(".");
    let cur = splitted[0];
    const result = [cur];
    for (let i = 1; i < splitted.length; i++) {
        cur = cur.concat(".", splitted[i])
        result.push(cur);
    }
    return result.reverse();
}

module.exports = {
    getEmitter
};
