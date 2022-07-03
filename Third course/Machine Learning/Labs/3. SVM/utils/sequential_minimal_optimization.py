import math

import numpy as np


# Set tolerances


class SMOModel:

    def __init__(self, X, y, C, kernel, eps=1.0e-8, tol=1.0e-10, kernel_matrix=None):
        self.X = X
        self.y = y
        self.C = C
        self.kernel = kernel
        self.m = len(self.X)
        self.alphas = self.__init_alphas()
        self.b = 0.0
        self.eps = eps
        self.tol = tol
        if kernel_matrix is None:
            self.kernel_matrix = self.__init_kernel_matrix()
        else:
            self.kernel_matrix = kernel_matrix

    def __init_alphas(self):
        return np.zeros(self.m)

    def __init_kernel_matrix(self):
        kernel_matrix = np.zeros((self.m, self.m))
        for i in range(self.m):
            kernel_matrix[:, i] = self.kernel(self.X, self.X[i, :])
        return kernel_matrix

    def take_step(self, i: int, j: int):
        eta = self.__calculate_eta(i, j)

        if eta >= 0:
            return

        L, H = self.__find_alpha_bounds(i, j)
        error_i = self.__calculate_error(i)
        error_j = self.__calculate_error(j)
        alpha_old_i = self.alphas[i]
        alpha_old_j = self.alphas[j]

        self.alphas[j] -= (self.y[j] * (error_i - error_j)) / eta
        if L < self.alphas[j] < H:
            self.alphas[j] = self.alphas[j]
        elif self.alphas[j] <= L:
            self.alphas[j] = L
        elif self.alphas[j] >= H:
            self.alphas[j] = H

        if self.alphas[j] < 1e-8:
            self.alphas[j] = 0.0
        elif self.alphas[j] > (self.C - 1e-8):
            self.alphas[j] = self.C

        self.alphas[i] = self.alphas[i] + self.y[i] * self.y[j] * (alpha_old_j - self.alphas[j])

        b1 = self.__calculate_b(error_i, i, alpha_old_i, j, alpha_old_j)
        b2 = self.__calculate_b(error_j, j, alpha_old_j, i, alpha_old_i)

        if 0 < self.alphas[i] < self.C:
            self.b = b1
        elif 0 < self.alphas[j] < self.C:
            self.b = b2
        else:
            self.b = (b1 + b2) * 0.5

        return

    def train(self):
        iters = 0
        max_iter = 1000
        while iters < max_iter:
            iters += 1
            alpha_prev = np.copy(self.alphas)
            for j in range(self.m):
                i = self.__random_index(j)
                self.take_step(i, j)
            diff = np.linalg.norm(self.alphas - alpha_prev)
            if diff < self.tol:
                break

    def predict_row(self, X_test: np.ndarray):
        return ((self.alphas * self.y) @ self.kernel(self.X, X_test)) + self.b
        # k_v = self.kernel(self.X, X_test)
        # return np.dot((self.alphas * self.y).T, k_v.T) + self.b

    def predict(self, X_test: np.ndarray):

        return np.sign(self.predict_row(X_test))

    def __random_index(self, z: int):
        i = z
        while i == z:
            i = np.random.randint(0, self.m - 1)
        return i

    def __find_alpha_bounds(self, i: int, j: int):
        L = None
        H = None
        if self.y[i] != self.y[j]:
            difference = self.alphas[i] - self.alphas[j]
            L = max(0, -difference)
            H = min(self.C, self.C - difference)
        elif self.y[i] == self.y[j]:
            difference = self.alphas[i] + self.alphas[j]
            L = max(0, difference - self.C)
            H = min(self.C, difference)
        return L, H

    def __calculate_eta(self, i: int, j: int):
        return 2.0 * self.kernel_matrix[i, j] - self.kernel_matrix[i, i] - self.kernel_matrix[j, j]

    def __calculate_error(self, i: int):
        return self.predict_row(self.X[i]) - self.y[i]

    def __calculate_b(self, error, i, alpha_old_i, j, alpha_old_j):
        return self.b - error - self.y[i] * (self.alphas[i] - alpha_old_i) * self.kernel_matrix[i, i] - self.y[j] * (self.alphas[j] - alpha_old_j) * self.kernel_matrix[i, j]
