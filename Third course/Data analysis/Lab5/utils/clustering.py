import numpy as np
import matplotlib.pyplot as plt
from sklearn.metrics import pairwise_distances_argmin, euclidean_distances
from sklearn.cluster import KMeans
from sklearn import metrics
from sklearn.metrics.cluster import rand_score


def run_handmadeKMeans(X):
    rng = np.random.RandomState(42)
    centers = [0, 4] + rng.randn(4, 2)

    def draw_points(ax, c, factor=1):
        ax.scatter(X[:, 0], X[:, 1], c=c, cmap='viridis',
                   s=50 * factor, alpha=0.3)

    def draw_centers(ax, centers, factor=1, alpha=1.0):
        ax.scatter(centers[:, 0], centers[:, 1],
                   c=np.arange(4), cmap='viridis', s=200 * factor,
                   alpha=alpha)
        ax.scatter(centers[:, 0], centers[:, 1],
                   c='black', s=50 * factor, alpha=alpha)

    def make_ax(fig, gs):
        ax = fig.add_subplot(gs)
        ax.xaxis.set_major_formatter(plt.NullFormatter())
        ax.yaxis.set_major_formatter(plt.NullFormatter())
        return ax

    fig = plt.figure(figsize=(15, 4))
    gs = plt.GridSpec(4, 15, left=0.02, right=0.98, bottom=0.05, top=0.95, wspace=0.2, hspace=0.2)
    ax0 = make_ax(fig, gs[:4, :4])
    ax0.text(0.98, 0.98, "Random Initialization", transform=ax0.transAxes,
             ha='right', va='top', size=16)
    draw_points(ax0, 'gray', factor=2)
    draw_centers(ax0, centers, factor=2)

    for i in range(3):
        ax1 = make_ax(fig, gs[:2, 4 + 2 * i:6 + 2 * i])
        ax2 = make_ax(fig, gs[2:, 5 + 2 * i:7 + 2 * i])

        # E-step
        y_pred = pairwise_distances_argmin(X, centers)
        draw_points(ax1, y_pred)
        draw_centers(ax1, centers)

        # M-step
        new_centers = np.array([X[y_pred == i].mean(0) for i in range(4)])
        draw_points(ax2, y_pred)
        draw_centers(ax2, centers, alpha=0.3)
        draw_centers(ax2, new_centers)
        for i in range(4):
            ax2.annotate('', new_centers[i], centers[i],
                         arrowprops=dict(arrowstyle='->', linewidth=1))

        # Finish iteration
        centers = new_centers
        ax1.text(0.95, 0.95, "E-Step", transform=ax1.transAxes, ha='right', va='top', size=14)
        ax2.text(0.95, 0.95, "M-Step", transform=ax2.transAxes, ha='right', va='top', size=14)

    # Final E-step
    y_pred = pairwise_distances_argmin(X, centers)
    axf = make_ax(fig, gs[:4, -4:])
    draw_points(axf, y_pred, factor=2)
    draw_centers(axf, centers, factor=2)
    axf.text(0.98, 0.98, "Final Clustering", transform=axf.transAxes,
             ha='right', va='top', size=16)


def run_KMeans(X, n_clusters):
    y_pred = KMeans(n_clusters=n_clusters, random_state=42).fit_predict(X)
    plt.figure(figsize=(16, 16))
    plt.scatter(X[:, 0], X[:, 1], c=y_pred, edgecolor='k')
    return KMeans(n_clusters=n_clusters, random_state=42).fit(X)


def metrica(X_reduced, y_target, kmeans):
    print(rand_score(y_target, kmeans.labels_))
    print(metrics.silhouette_score(X_reduced, kmeans.labels_, metric='euclidean'))


def distances(X_reduced, kmeans):
    in_dist = np.mean(
        [euclidean_distances([kmeans.cluster_centers_[kmeans.labels_[idx]]], [point]) for idx, point in
         enumerate(X_reduced)])

    dists = euclidean_distances(kmeans.cluster_centers_)
    tri_dists = dists[np.triu_indices(5, 1)]
    max_dist, avg_dist, min_dist = tri_dists.max(), tri_dists.mean(), tri_dists.min()
    out_dist = avg_dist
    print(in_dist)
    print(out_dist)
