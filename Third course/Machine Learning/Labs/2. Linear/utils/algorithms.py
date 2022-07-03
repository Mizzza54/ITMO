import random

import numpy as np

epsilon = 1e-6


def init_weights(count_objects: int, count_features: int) -> np.ndarray:
    w = np.zeros(count_features)
    random.seed(0)
    for i in range(count_features):
        w[i] = random.uniform(-0.5 / count_objects, 0.5 / count_objects)
    return w


def init_l(X, Y, w, function):
    Q = 0
    for i in range(len(X)):
        Q += function(np.array([X[i].dot(w)]), np.array([Y[i]]))
    return Q / len(X)


def stochastic_gradient_descent(X: np.ndarray, Y: np.ndarray, tau, function, derivative, alpha, init_step,
                                iterations_limit):
    random.seed(0)
    count_objects = len(X)
    count_features = len(X[0])
    iterations = 0
    w = init_weights(count_objects, count_features)
    Q = init_l(X, Y, w, function)
    step = init_step
    logs = []
    while True:
        k = iterations % count_objects
        x = X[k]
        y = Y[k]
        wx = np.array([w.dot(x)])
        l_tau = function(np.array([y]), wx) + (tau / 2) * (np.linalg.norm(w) ** 2)
        logs.append(l_tau)
        gradient = derivative(y, wx[0], x)
        for i in range(count_features):
            w[i] = w[i] * (1 - step * tau) - step * gradient[i]
        Q_prev = Q
        Q = (1 - alpha) * Q + alpha * l_tau
        iterations += 1
        step = init_step / iterations
        if iterations >= iterations_limit or abs(Q - Q_prev) < epsilon:
            return w, logs


def least_squares_approximation(X: np.ndarray, Y: np.ndarray, tau: float) -> np.ndarray:
    temp, _ = X.T.shape
    return np.linalg.inv(X.T @ X + np.identity(temp).dot(tau)) @ X.T @ Y


def apply_error_function(X, Y, w, function):
    Y_real = np.zeros(len(X))
    Y_predict = np.zeros(len(X))
    for i in range(len(X)):
        x = X[i]
        y_real = Y[i]
        y_predict = x.dot(w)
        Y_real[i] = y_real
        Y_predict[i] = y_predict
    return function(Y_real, Y_predict)
