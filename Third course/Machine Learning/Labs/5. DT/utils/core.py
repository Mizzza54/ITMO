import numpy as np
import pandas as pd
from matplotlib import pyplot as plt

from preprocessing import preprocessing_initial_data
from ada_boost_classifier import AdaBoostClassifier
from random_forest_classifier import RandomForestClassifier
from decision_tree import DecisionTreeClassifier


# from sklearn.ensemble import RandomForestClassifier




from typing import Optional


def plot_adaboost(X: np.ndarray,
                  y: np.ndarray,
                  clf=None,
                  sample_weights: Optional[np.ndarray] = None,
                  annotate: bool = False,
                  ax=None) -> None:
    """ Plot ± samples in 2D, optionally with decision boundary """


    if not ax:
        fig, ax = plt.subplots(figsize=(5, 5), dpi=100)
        fig.set_facecolor('white')

    pad = 1
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
        plot_step = 0.1
        xx, yy = np.meshgrid(np.arange(x_min, x_max, plot_step),
                             np.arange(y_min, y_max, plot_step))

        print(np.c_[xx.ravel(), yy.ravel()])
        Z = np.array(clf.predict(np.c_[xx.ravel(), yy.ravel()]))
        Z = Z.reshape(xx.shape)

        # If all predictions are positive class, adjust color map acordingly
        if list(np.unique(Z)) == [1]:
            fill_colors = ['r']
        else:
            fill_colors = ['b', 'r']

        ax.contourf(xx, yy, Z, colors=fill_colors, alpha=0.2)

    if annotate:
        for i, (x, y) in enumerate(X):
            offset = 0.05
            ax.annotate(f'$x_{i + 1}$', (x + offset, y - offset))

    ax.set_xlim(x_min+0.5, x_max-0.5)
    ax.set_ylim(y_min+0.5, y_max-0.5)
    ax.set_xlabel('$x_1$')
    ax.set_ylabel('$x_2$')







def accuracy(y_true, y_pred):
    accuracy = np.sum(y_true == y_pred) / len(y_true)
    return accuracy


df_chips = pd.read_csv('../data/raw/chips.csv')
df_geyser = pd.read_csv('../data/raw/geyser.csv')
preprocessing_initial_data(df_chips)
preprocessing_initial_data(df_geyser)
X_Geyser = df_geyser.values[:, :-1]
Y_Geyser = df_geyser.values[:, -1]

# random_fores_classifier = RandomForestClassifier()
# random_fores_classifier.fit(X_Geyser, Y_Geyser)
# y_pred = random_fores_classifier.predict(X_Geyser)
# acc = accuracy(Y_Geyser, y_pred)
#
# print("Accuracy:", acc)


ada_boost = RandomForestClassifier()
ada_boost.fit(X_Geyser, Y_Geyser)
y_pred = ada_boost.predict(X_Geyser)
acc = accuracy(Y_Geyser, y_pred)

# 0.954954954954955

# x = ada_boost.predict([[0.46362489, 0.78210833], [0.38786935, 0.77432611]])
# print(type(X_Geyser))
# print(type(x))

print("Accuracy:", acc)



# import numpy as np
# from sklearn import datasets
# from sklearn.model_selection import train_test_split
# from decision_tree import DecisionTreeClassifier
#

# data = datasets.load_breast_cancer()
# X, y = data.data, data.target
#
# X = X[:, 0:1]
#
# clf = DecisionTreeClassifier(max_depth=10)
# clf.fit(X, y)
#
# y_pred = clf.predict(X)
# acc = accuracy(y, y_pred)
#
# print("Accuracy:", acc)

plot_adaboost(X_Geyser, Y_Geyser, ada_boost)
plt.show()