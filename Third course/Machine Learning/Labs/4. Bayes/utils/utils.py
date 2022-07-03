import os

import numpy as np

from nltk.util import ngrams

path2parts = './data/messages/part'
legit_label = 'legit'
spam_label = 'spmsg'


def generate_ngrams(text: str, n: int) -> list:
    tokens = [token for token in text.split(" ") if token != ""]
    return list(ngrams(tokens, n))


def read_all_parts(parts_count: int, n: int):
    X = []
    Y = []
    for i in range(1, parts_count + 1):
        current_X, current_Y = read_part(i, n)
        X.append(current_X)
        Y.append(current_Y)
    return X, Y


def read_part(idx: int, n: int) -> (list, list):
    path = path2parts + str(idx)
    X = []
    Y = []
    for filename in os.listdir(path):
        with open(path + "/" + filename, 'r') as file:
            subject = list(map(int, file.readline().split()[1:]))
            file.readline()
            text = list(map(int, file.readline().split()))
            subject.extend(text)
            ngram = generate_ngrams(' '.join(str(x) for x in subject), n)
            X.append(ngram)
            if legit_label in filename:
                Y.append(0)
            elif spam_label in filename:
                Y.append(1)
    return X, Y
