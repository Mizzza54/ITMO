import numpy as np
from sklearn.model_selection import KFold
from sklearn.metrics import accuracy_score

from utils import kernels
from utils.sequential_minimal_optimization import SMOModel

C_set = [0.05, 0.1, 0.5, 1.0, 5.0, 10.0, 50.0, 100.0]
degree_set = [2, 3, 4, 5]
beta_set = [1, 2, 3, 4, 5]
kernels_set = [kernels.LinearKernel, kernels.PolynomialKernel, kernels.GaussianKernel]


def cross_validation(X: np.ndarray, y: np.ndarray, C: float, kernel, kernel_matrix=None):
    kf = KFold(n_splits=4)
    accuracy = []
    for train_index, test_index in kf.split(X):
        X_train, X_test = X[train_index], X[test_index]
        y_train, y_test = y[train_index], y[test_index]
        model = SMOModel(X_train, y_train, C, kernel, kernel_matrix=kernel_matrix)
        model.train()
        y_prediction = np.apply_along_axis(lambda x: model.predict(x), 1, X_test)
        accuracy.append(accuracy_score(y_test, y_prediction))
    return np.average(np.array(accuracy))


def hyperparameter_optimization(X: np.ndarray, y: np.ndarray):
    kernel_best = None
    C_best = None
    accuracy_best = -1
    kernel_matrices = init_kernel_matrices(X)
    for C in C_set:
        for key in kernel_matrices.keys():
            kernel_matrix, kernel, parameter = kernel_matrices.get(key)
            accuracy = cross_validation(X, y, C, kernel, kernel_matrix=kernel_matrix)
            if accuracy > accuracy_best:
                accuracy_best = accuracy
                kernel_best = kernel
                C_best = C
            print("----------")
            print("Kernel: ", repr(kernel))
            print("Parameter: ", parameter)
            print("Accuracy: ", accuracy)
            print("C: ", C)
    return C_best, kernel_best,


def init_kernel_matrix(X: np.ndarray, m: int, kernel):
    kernel_matrix = np.zeros((m, m))
    for i in range(m):
        kernel_matrix[:, i] = kernel(X, X[i, :])
    return kernel_matrix


def init_kernel_matrices(X: np.ndarray):
    kernel_matrices = dict()
    for kernel in kernels_set:
        parameters = None
        if kernel == kernels.LinearKernel:
            parameters = [None]
        elif kernel == kernels.PolynomialKernel:
            parameters = degree_set
        elif kernel == kernels.GaussianKernel:
            parameters = beta_set

        for parameter in parameters:
            init_kernel = kernel(parameter)
            kernel_matrix = init_kernel_matrix(X, len(X), init_kernel)
            name = repr(init_kernel) + '_' + str(parameter)
            kernel_matrices[name] = (kernel_matrix, init_kernel, parameter)
    return kernel_matrices
