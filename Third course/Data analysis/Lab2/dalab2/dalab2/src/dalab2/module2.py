from typing import List, Tuple, Union
from collections import Counter
import numpy as np
from matplotlib import pyplot as plt


def fast_hist(array: List[Union[int, float]],
              bins: int) -> Tuple[List[int], List[float]]:
    """
    Builds bins' labels and bins' value counts for given array
    :param array: array with numeric values
    :param bins:  number of bins in result distribution
    :return: Two lists:
             first contains value counts of each bin,
             second contains list of bins' labels
    """

    dict_unique = Counter(array)
    unique_counts = []
    unique = sorted(dict_unique)
    for i in sorted(dict_unique):
        unique_counts.append(dict_unique[i])
    min_elem = min(array)
    max_elem = max(array)
    step = (max_elem - min_elem) / bins
    labels = np.arange(min_elem, max_elem + step, step)
    value_counts = [0] * (len(labels) - 1)
    index_unique = 0
    index_labels = 0
    while True:
        if index_labels == len(labels) - 1:
            break
        if index_unique == len(unique):
            break

        if (labels[index_labels] <= unique[index_unique]) and \
                (unique[index_unique] < labels[index_labels + 1]):
            value_counts[index_labels] += dict_unique[unique[index_unique]]
            index_unique += 1
        else:
            index_labels += 1
    value_counts[-1] += dict_unique[unique[-1]]
    return value_counts, labels


def draw(array):
    value_counts, bins_names = fast_hist(array, len(set(array)))
    y_pos = np.arange(len(value_counts))
    plt.bar(y_pos, value_counts)
    plt.xticks(y_pos, bins_names[0:-1])
    plt.show()
    print('Значения колонок:', value_counts)
    print('Названия колонок:', bins_names)
