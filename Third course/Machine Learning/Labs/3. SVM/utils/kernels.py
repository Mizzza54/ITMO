import scipy.spatial.distance as dist
import numpy as np


class LinearKernel(object):
    def __init__(self, ignore):
        self.ignore = ignore

    def __call__(self, x: np.ndarray, y: np.ndarray):
        return np.dot(x, y.T)

    def __repr__(self):
        return "Linear kernel"


class PolynomialKernel(object):
    def __init__(self, degree=2):
        self.degree = degree

    def __call__(self, x: np.ndarray, y: np.ndarray):
        return np.dot(x, y.T) ** self.degree

    def __repr__(self):
        return "Polynomial kernel"


class GaussianKernel(object):
    def __init__(self, gamma=0.1):
        self.gamma = gamma

    def __call__(self, x: np.ndarray, y: np.ndarray):
        x = np.atleast_2d(x)
        y = np.atleast_2d(y)
        return np.exp(-self.gamma * dist.cdist(x, y) ** 2).flatten()

    def __repr__(self):
        return "Gaussian kernel"
