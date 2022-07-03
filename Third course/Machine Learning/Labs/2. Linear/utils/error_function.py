import numpy as np


def sum_of_squares(real: np.ndarray, predict: np.ndarray) -> float:
    return np.sum((real - predict) ** 2)


def mean_square_error(real: np.ndarray, predict: np.ndarray) -> float:
    return sum_of_squares(real, predict) / len(real)


def symmetric_mean_absolute_percentage_error(real: np.ndarray, predict: np.ndarray) -> float:
    # if np.abs(x) + np.abs(y) == 0:
    #     return 0.01
    # else:
    #     return (100 / len(x)) * np.sum(2 * np.abs(x - y) / (np.abs(x) + np.abs(y)))
    res = 0
    for i in range(len(real)):
        if (abs(real[i]) + abs(predict[i])) != 0:
            res += abs(real[i] - predict[i]) / (abs(real[i]) + abs(predict[i]))
    return (100 / len(real)) * res


def mean_square_error_gradient(real, predict, x):
    gradient = []
    for i in range(len(x)):
        gradient.append(2 * (predict - real) * x[i])
    return np.array(gradient)


def symmetric_mean_absolute_percentage_error_gradient(real, predict, x):
    # gradient = []
    # for i in range(len(x)):
    #     numerator = abs(real - predict)
    #     denominator = abs(real) + abs(predict)
    #     if denominator == 0:
    #         return 0
    #     module1 = x[i] * numerator / float((real - predict))
    #     module2 = x[i] * real / float(abs(real))
    #     gradient.append((module1 * denominator - numerator * module2) / float(denominator * denominator))
    # return np.array(gradient)
    gradient = []
    for i in range(len(x)):
        part1 = (predict - real) / (abs(real - predict) * (abs(real) + abs(predict)))
        part2 = (predict * abs(real - predict)) / (abs(predict) * ((abs(real) + abs(predict)) ** 2))
        gradient.append((part1 - part2) * x[i])
    return np.array(gradient)


def root_mean_square_error(real: np.ndarray, predict: np.ndarray) -> float:
    return mean_square_error(real, predict) ** 0.5


def normalized_root_mean_square_error(real: np.ndarray, predict: np.ndarray) -> float:
    return root_mean_square_error(real, predict) / (max(real) - min(real))
