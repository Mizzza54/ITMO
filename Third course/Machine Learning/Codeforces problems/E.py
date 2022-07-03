import numpy as np
from collections import defaultdict


class Message:
    def __init__(self, label, wordsMessage):
        self.label = label
        self.words = wordsMessage


class NaiveBayesClassifier:
    def __init__(self, Messages, lambdasInput, alphaInput, classes_count):
        self.Messages = Messages
        self.lambdas = defaultdict(lambda: 0)
        self.alpha = alphaInput
        self.classes_count = defaultdict(lambda: 0)
        for idx in range(classes_count):
            self.classes_count[idx + 1] = 0

        self.frequencies = defaultdict(lambda: 0)
        self.probabilities_classes = defaultdict(lambda: 0.0)
        self.conditional_probabilities = defaultdict(lambda: defaultdict(lambda: 0.0))
        self.words = set()
        self.Q = 2

        for idx in range(len(Messages)):
            message = set(Messages[idx].words)
            message_label = Messages[idx].label
            self.classes_count[message_label] += 1
            for word in message:
                self.words.add(word)
                self.frequencies[message_label, word] += 1

        for idx in self.classes_count.keys():
            self.lambdas[idx] = lambdasInput[idx - 1]
            self.probabilities_classes[idx] = np.log(self.lambdas[idx]) + np.log(self.classes_count[idx] / len(self.Messages))
            for jdx in self.words:
                self.conditional_probabilities[idx][jdx] = (self.frequencies.get((idx, jdx), 0) + self.alpha) / (
                            self.classes_count[idx] + self.Q * self.alpha)

        # print('lambdas = ', str(self.lambdas))
        # print('classes_count = ', str(self.classes_count))
        # print('frequencies = ', str(self.frequencies))
        # print('probabilities_classes = ', str(self.probabilities_classes))
        # print('conditional_probabilities', str(self.conditional_probabilities))

    def predict(self, testData):
        answer = []
        for test_line in testData:
            set_test_line = set(test_line.words)
            probability = []
            general_sum = 0
            for idx in self.classes_count.keys():
                prob = self.probabilities_classes[idx]

                for word in self.words:
                    temp = self.conditional_probabilities[idx][word]

                    if word in set_test_line:
                        prob += np.log(temp)
                    else:
                        prob += np.log(1 - temp)

                probability.append(prob)

            temp_list = []
            eps = 0.0001
            maxElem = max(probability)
            for idx in range(len(probability)):
                if abs(probability[idx]) <= eps:
                    probability[idx] = 0
                else:
                    probability[idx] = np.exp(probability[idx] - maxElem)
                    general_sum += probability[idx]

            for idx in range(len(probability)):
                temp_list.append(probability[idx] / general_sum)

            answer.append(temp_list.copy())
        return answer


k = int(input())
lambdas = list(map(int, input().split()))
alpha = int(input())
n = int(input())
train = []
for _ in range(n):
    line = input().split()
    c = int(line[0])
    words = line[2:]
    train.append(Message(c, words))
m = int(input())
test = []
for _ in range(m):
    line = input().split()
    words = line[1:]
    test.append(Message(None, words))

model = NaiveBayesClassifier(train, lambdas, alpha, k)
result = model.predict(test)
for i in range(len(result)):
    for j in range(len(result[i])):
        print(round(result[i][j], 11), end=' ')
    print()
