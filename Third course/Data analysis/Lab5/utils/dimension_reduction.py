import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA
import umap


def run_PCA(df: pd.DataFrame, y_target: pd.Series):
    pca = PCA(n_components=2)
    X_reduced = pca.fit_transform(df)

    print("Meaning of the 2 components:")
    for component in pca.components_:
        print(" + ".join("%.3f x %s" % (value, name)
                         for value, name in zip(component,
                                                y_target)))
    plt.figure(figsize=(10, 7))
    plt.scatter(X_reduced[:, 0], X_reduced[:, 1], c=y_target, s=70, edgecolor="k")
    plt.show()
    return X_reduced


def run_UMAP(df: pd.DataFrame, y_target: pd.Series, n_neighbors, min_dist):
    reducer = umap.UMAP(n_neighbors=n_neighbors, min_dist=min_dist, random_state=42)
    reducer.fit(df)

    X_reduced = reducer.transform(df)
    # Verify that the result of calling transform is
    # idenitical to accessing the embedding_ attribute
    assert (np.all(X_reduced == reducer.embedding_))

    # plt.scatter(embedding[:, 0], embedding[:, 1], c=digits.target, cmap='Spectral', s=5)
    # plt.gca().set_aspect('equal', 'datalim')
    # plt.colorbar(boundaries=np.arange(11)-0.5).set_ticks(np.arange(10))
    # plt.title('UMAP projection of the Digits dataset', fontsize=24);

    print('Projecting %d-dimensional data to 2D' % df.shape[1])

    plt.figure(figsize=(14, 12))
    plt.scatter(X_reduced[:, 0], X_reduced[:, 1], c=y_target,
                edgecolor='none', alpha=0.7, s=40,
                cmap=plt.cm.get_cmap('nipy_spectral', 10))
    plt.colorbar()
    plt.show()
    return X_reduced
