import matplotlib.pyplot as plt
import numpy as np
from sklearn.metrics import roc_curve
from sklearn.metrics import accuracy_score


def plot_receiver_operating_characteristic(X, Y, model):
    y_pred_proba = []

    for x in [item for sublist in X for item in sublist]:
        pred, _ = model.predict(x)
        y_pred_proba.append(pred)

    fpr, tpr, _ = roc_curve([item for sublist in Y for item in sublist], y_pred_proba)

    plt.plot(fpr, tpr)
    plt.ylabel('True Positive Rate')
    plt.xlabel('False Positive Rate')
    plt.show()


def plot_legit_lambda_accuracy(X, Y, model, legit_lambda_from, legit_lambda_to):
    X = [item for sublist in X for item in sublist]
    Y = [item for sublist in Y for item in sublist]
    legit_lambda_set = [10 ** i for i in range(legit_lambda_from, legit_lambda_to)]
    accuracy = []
    for legit_lambda in legit_lambda_set:
        model.lambdas = [legit_lambda, 1]
        y_predictions = []
        for i in range(len(X)):
            elem = X[i]
            y_prediction, _ = model.predict(elem)
            y_predictions.append(y_prediction)
        accuracy.append(accuracy_score(Y, y_predictions))

    plt.figure(figsize=(16, 9))
    plt.grid(linestyle='--')
    plt.plot(legit_lambda_set, accuracy, linestyle='-', marker='.', color='red', label='Accuracy')
    plt.xlabel('legit lambda')
    plt.ylabel('accuracy')
    plt.legend()
    plt.show()


def plot_legit_lambda_legit_accuracy(X, Y, model, legit_lambda_from, legit_lambda_to):
    X = [item for sublist in X for item in sublist]
    Y = [item for sublist in Y for item in sublist]

    X_new = []
    for i in range(len(X)):
        if Y[i] == 0:
            X_new.append(X[i])
    Y_new = np.zeros(len(X_new))

    legit_lambda_set = [10 ** i for i in range(legit_lambda_from, legit_lambda_to)]
    accuracy = []
    for legit_lambda in legit_lambda_set:
        model.lambdas = [legit_lambda, 1]
        y_predictions = []
        for i in range(len(X_new)):
            elem = X_new[i]
            y_prediction, _ = model.predict(elem)
            y_predictions.append(y_prediction)
        accuracy.append(accuracy_score(Y_new, y_predictions))

    plt.figure(figsize=(16, 9))
    plt.grid(linestyle='--')
    plt.plot(legit_lambda_set, accuracy, linestyle='-', marker='.', color='red', label='Accuracy')
    plt.xlabel('legit lambda')
    plt.ylabel('accuracy')
    plt.legend()
    plt.show()
