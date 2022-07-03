from utils.algorithms import stochastic_gradient_descent, apply_error_function, least_squares_approximation
from utils.error_function import symmetric_mean_absolute_percentage_error


def objective_sgd(trial, X_train, Y_train, X_test, Y_test, function, gradient):
    tau = trial.suggest_uniform("tau", 0.001, 20)
    step = trial.suggest_uniform("step", 0.001, 0.1)
    alpha = trial.suggest_uniform("alpha", 0.1, 0.9)

    fit, _ = stochastic_gradient_descent(X_train, Y_train, tau, function, gradient, alpha, step, 2000)

    error = apply_error_function(X_test, Y_test, fit, symmetric_mean_absolute_percentage_error)

    return error


def objective_lsa(trial, X_train, Y_train, X_test, Y_test, function):
    tau = trial.suggest_uniform("tau", 1e-9, 1)

    fit = least_squares_approximation(X_train, Y_train, tau)

    error = apply_error_function(X_test, Y_test, fit, function)

    return error
