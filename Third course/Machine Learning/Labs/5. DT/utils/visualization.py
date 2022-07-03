import numpy as np
import matplotlib.pyplot as plt
from pandas import DataFrame


def visualization_initial_data(df: DataFrame, title: str):
    colors = {1: 'red', 0: 'blue'}
    plt.title(title)
    plt.xlabel('x-label')
    plt.ylabel('y-label')
    plt.scatter(df.x, df.y, c=df['class'].map(colors))


def plot_model1(model, X, y, ax):
    # X = np.append(X, [[0.3, 0.39]], axis=0)
    # y = np.append(y, [0], axis=0)

    xrange = np.linspace(X[:, 0].min(), X[:, 0].max(), 100)
    yrange = np.linspace(X[:, 1].min(), X[:, 1].max(), 100)

    grid = [[model.predict(np.array([[xr, yr]])) for xr in xrange] for yr in yrange]
    grid = np.array(grid).reshape(len(xrange), len(yrange))

    predictions = np.array(model.predict(X))

    colormap = np.array(['r', 'b'])
    ax.scatter(X[:, 0], X[:, 1], marker="s", s=200, c=colormap[y.astype(int)], label='real')
    ax.scatter(X[:, 0], X[:, 1], marker="o", s=50, c=colormap[predictions.astype(int)], label='predict')
    ax.legend(loc='upper left')
    ax.contour(xrange, yrange, grid, linewidths=1, linestyles='-', colors='y')

    return grid, ax


from typing import Optional


def plot_model2(X: np.ndarray,
                  y: np.ndarray,
                  clf=None,
                  sample_weights: Optional[np.ndarray] = None,
                  ax=None):

    pad = 0.5
    x_min, x_max = X[:, 0].min() - pad, X[:, 0].max() + pad
    y_min, y_max = X[:, 1].min() - pad, X[:, 1].max() + pad

    if sample_weights is not None:
        sizes = np.array(sample_weights) * X.shape[0] * 100
    else:
        sizes = np.ones(shape=X.shape[0]) * 100

    X_pos = X[y == 1]
    sizes_pos = sizes[y == 1]
    ax.scatter(*X_pos.T, s=sizes_pos, marker='+', color='red')

    X_neg = X[y == 0]
    sizes_neg = sizes[y == 0]
    ax.scatter(*X_neg.T, s=sizes_neg, marker='.', c='blue')

    if clf:
        plot_step = 0.01
        xx, yy = np.meshgrid(np.arange(x_min, x_max, plot_step),
                             np.arange(y_min, y_max, plot_step))

        Z = np.array(clf.predict(np.c_[xx.ravel(), yy.ravel()]))
        Z = Z.reshape(xx.shape)

        ax.contourf(xx, yy, Z, alpha=0.2)

    ax.set_xlim(x_min+0.5, x_max-0.5)
    ax.set_ylim(y_min+0.5, y_max-0.5)
    return ax
