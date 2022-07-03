from typing import Tuple, List

import numpy as np


class Object:
    def __init__(self, features: np.ndarray, target: float = None):
        self.features = features
        self.target = target


def load_file(path: str) -> Tuple[List[Object], List[Object]]:
    with open(path) as file:
        objects_train = []
        objects_test = []
        count_features = int(file.readline())
        count_train_objects = int(file.readline())
        for i in range(count_train_objects):
            features = [float(number) for number in file.readline().split(' ')]
            target = features.pop()
            objects_train.append(Object(np.array(features), target))
        count_test_objects = int(file.readline())
        for i in range(count_test_objects):
            features = [float(number) for number in file.readline().split(' ')]
            target = features.pop()
            objects_test.append(Object(np.array(features), target))
        return objects_train, objects_test


def calculate_ema(list, alpha):
    ema = [0]
    for elem in list:
        ema.append((elem * alpha) + ema[-1] * (1 - alpha))
    return ema
