import math

import numpy as np

from sklearn.metrics import accuracy_score

from utils import core

alphas_set = [10 ** i for i in range(-10, 3)]
legit_lambda_set = [10 ** i for i in range(290, 300)]


def cross_validation(models, alpha, legit_lambda):
    accuracy = []
    legit_msg_wrong_counter = 0
    for model, X_test, Y_test in models:
        model.alpha = alpha
        model.lambdas = [legit_lambda, 1]
        y_predictions = []
        for i in range(len(X_test)):
            elem = X_test[i]
            y_prediction, _ = model.predict(elem)
            y_predictions.append(y_prediction)
            if Y_test[i] == 0 and y_prediction == 1:
                legit_msg_wrong_counter += 1
        accuracy.append(accuracy_score(Y_test, y_predictions))
    return np.average(np.array(accuracy)), legit_msg_wrong_counter


def alpha_optimization(models):
    alpha_best = None
    accuracy_best = -1
    for alpha in alphas_set:
        accuracy, _ = cross_validation(models, alpha, 1)
        if accuracy > accuracy_best:
            accuracy_best = accuracy
            alpha_best = alpha
        print("----------")
        print("Alpha: ", alpha)
        print("Accuracy: ", accuracy)
    return accuracy_best, alpha_best


def lambda_optimization(models, alpha):
    legit_lambda_best = None
    accuracy_best = -1
    for legit_lambda in legit_lambda_set:
        accuracy, count = cross_validation(models, alpha, legit_lambda)
        if accuracy > accuracy_best and count == 0:
            accuracy_best = accuracy
            legit_lambda_best = legit_lambda
        print("----------")
        print("Legit lambda 1e{}".format(math.log10(legit_lambda)))
        print("Legit lambda: ", )
        print("Count: ", count)
        print("Accuracy: ", accuracy)
    return accuracy_best, legit_lambda_best


def init_models(X, Y):
    models = []
    for index in range(len(X)):
        X_test, Y_test = X[index], Y[index]
        X_copy, Y_copy = X.copy(), Y.copy()

        X_copy.pop(index)
        Y_copy.pop(index)

        X_train = [item for sublist in X_copy for item in sublist]
        Y_train = [item for sublist in Y_copy for item in sublist]

        model = core.NaiveBayesClassifier(X_train, Y_train, [1, 1], 1)
        models.append((model, X_test, Y_test))
    return models
