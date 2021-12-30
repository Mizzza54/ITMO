'use strict';

const fetch = require('node-fetch');

const API_KEY = require('./key.json');
const {json} = require("mocha/lib/reporters");

/**
 * @typedef {object} TripItem Город, который является частью маршрута.
 * @property {number} geoid Идентификатор города
 * @property {number} day Порядковое число дня маршрута
 */

class TripBuilder {

  constructor(geoids) {
    this.geoids = geoids;
    this.maxDays = 7;
    this.queueConditions = [];
  }

  addCondition(daysCount, condition) {
    for (let i = 0; i < daysCount; i++) {
      this.queueConditions.push(condition);
    }
  }

  /**
   * Метод, добавляющий условие наличия в маршруте
   * указанного количества солнечных дней
   * Согласно API Яндекс.Погоды, к солнечным дням
   * можно приравнять следующие значения `condition`:
   * * `clear`;
   * * `partly-cloudy`.
   * @param {number} daysCount количество дней
   * @returns {object} Объект планировщика маршрута
   */
  sunny(daysCount) {
    this.addCondition(daysCount, "sunny");
    return this;
  }

  /**
   * Метод, добавляющий условие наличия в маршруте
   * указанного количества пасмурных дней
   * Согласно API Яндекс.Погоды, к солнечным дням
   * можно приравнять следующие значения `condition`:
   * * `cloudy`;
   * * `overcast`.
   * @param {number} daysCount количество дней
   * @returns {object} Объект планировщика маршрута
   */
  cloudy(daysCount) {
    this.addCondition(daysCount, "cloudy");
    return this;
  }

  /**
   * Метод, добавляющий условие максимального количества дней.
   * @param {number} daysCount количество дней
   * @returns {object} Объект планировщика маршрута
   */
  max(daysCount) {
    this.maxDays = daysCount;
    return this;
  }

  /**
   * Метод, возвращающий Promise с планируемым маршрутом.
   * @returns {Promise<TripItem[]>} Список городов маршрута
   */
  async build() {
    const forecasts = await fetchWeatherForecast(this.geoids);
    const spentDays = new Map();
    this.geoids.forEach(x => {
      spentDays.set(x, 0);
    })

    const route = this.findRoute(forecasts, spentDays, 0, null, []);

    if (route !== null) {
      return route;
    } else {
      throw new Error("Не могу построить маршрут!");
    }
  }

  findRoute(forecasts, spentDays, numOfDay, prevCity, route) {
    if (this.queueConditions.length === numOfDay) {
      return route;
    }
    const currentCondition = this.queueConditions[numOfDay];

    if (prevCity === null) {
      for (const key of forecasts.keys()) {
        if (forecasts.get(key)[numOfDay] === currentCondition) {
          route.push({ geoid: key, day: numOfDay + 1});
          spentDays.set(key, spentDays.get(key) + 1);
          return this.findRoute(forecasts, spentDays, numOfDay + 1, key, route)
        }
      }
      // reject promise
      return null;
    } else {
      if (spentDays.get(prevCity) < this.maxDays && forecasts.get(prevCity)[numOfDay] === currentCondition) {
        route.push({geoid: prevCity, day: numOfDay + 1});
        spentDays.set(prevCity, spentDays.get(prevCity) + 1);
        return this.findRoute(forecasts, spentDays, numOfDay + 1, prevCity, route)
      } else {
        for (const key of forecasts.keys()) {
          if (forecasts.get(key)[numOfDay] === currentCondition && spentDays.get(key) === 0) {
            route.push({ geoid: key, day: numOfDay + 1});
            spentDays.set(key, spentDays.get(key) + 1);
            return this.findRoute(forecasts, spentDays, numOfDay + 1, key, route)
          }
        }
        // reject promise
        return null;
      }
    }
  }
}

async function fetchWeatherForecast (geoids) {

  function mapWeatherCondition(x) {
    if (x === "clear" || x === "partly-cloudy") {
      return "sunny";
    }

    if (x === "cloudy" || x === "overcast") {
      return "cloudy";
    }

    return "other";
  }

  const result = new Map();

  for (const id of geoids) {
    const url = `https://api.weather.yandex.ru/v2/forecast?geoid=${id}&hours=false&limit=7`;
    await fetch(url, {headers: {"X-Yandex-API-Key": API_KEY.key}})
      .then(async response => {
        if (response.status === 200) {
          const jsonResponse = await response.json();
          result.set(id, jsonResponse.forecasts.map(d => {
            return mapWeatherCondition(d.parts.day_short.condition);
          }));
        } else {
          throw new Error(response.status);
        }
      })
  }
  return result;
}

/**
 * Фабрика для получения планировщика маршрута.
 * Принимает на вход список идентификаторов городов, а
 * возвращает планировщик маршрута по данным городам.
 *
 * @param {number[]} geoids Список идентификаторов городов
 * @returns {TripBuilder} Объект планировщика маршрута
 * @see https://yandex.ru/dev/xml/doc/dg/reference/regions-docpage/
 */
function planTrip(geoids) {
  return new TripBuilder(geoids);
}

module.exports = {
  planTrip
};
