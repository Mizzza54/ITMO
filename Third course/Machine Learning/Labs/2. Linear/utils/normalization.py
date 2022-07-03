from typing import List, Tuple

from utils.core import Object


def find_minmax_feature(objects: List[Object], index: int) -> Tuple[int, int]:
    min_val = objects[0].features[index]
    max_val = objects[0].features[index]
    for i in range(len(objects)):
        min_val = min(min_val, objects[i].features[index])
        max_val = max(max_val, objects[i].features[index])
    return min_val, max_val


def find_minmax_target(objects: List[Object]) -> Tuple[int, int]:
    min_val = objects[0].target
    max_val = objects[0].target
    for i in range(len(objects)):
        min_val = min(min_val, objects[i].target)
        max_val = max(max_val, objects[i].target)
    return min_val, max_val


def normalize_minmax(objects: List[Object]) -> List[Object]:
    for i in range(len(objects[0].features)):
        min_val, max_val = find_minmax_feature(objects, i)
        for j in range(len(objects)):
            if min_val == max_val:
                objects[j].features[i] = 0
            else:
                objects[j].features[i] = float(objects[j].features[i] - min_val) / float(max_val - min_val)

    for i in range(len(objects)):
        min_val, max_val = find_minmax_target(objects)
        if min_val == max_val:
            objects[i].target = 0
        else:
            objects[i].target = float(objects[i].target - min_val) / float(max_val - min_val)

    return objects
