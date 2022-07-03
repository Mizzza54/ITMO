from collections import defaultdict

import math


class NaiveBayesClassifier:
    def __init__(self, X, Y, lambdas, alpha):
        self.X = X
        self.Y = Y
        self.lambdas = lambdas
        self.alpha = alpha
        self.classes_count = defaultdict(lambda: 0)
        self.frequencies = defaultdict(lambda: 0)
        self.words = set()
        self.Q = 2
        for i in range(len(X)):
            message = X[i]
            message_type = Y[i]

            self.classes_count[message_type] += 1
            for word in message:
                self.words.add(word)
                self.frequencies[message_type, word] += 1

    def probability_class(self, class_type):
        return self.classes_count.setdefault(class_type, 0) / len(self.X)

    def predict(self, X_test):
        probability_legit = math.log(self.lambdas[0] * self.probability_class(0))
        probability_spam = math.log(self.lambdas[1] * self.probability_class(1))

        for word in X_test:
            cur_probability_legit = math.log((self.frequencies.get((0, word), 0) + self.alpha) / (self.classes_count[0] + 2 * self.alpha))
            cur_probability_spam = math.log((self.frequencies.get((1, word), 0) + self.alpha) / (self.classes_count[1] + 2 * self.alpha))

            probability_legit += cur_probability_legit
            probability_spam += cur_probability_spam

            # if word in X_test:
            #     probability_legit += cur_probability_legit
            #     probability_spam += cur_probability_spam
            # else:
            #     probability_legit += 1 - cur_probability_legit
            #     probability_spam += 1 - cur_probability_spam

        return 0 if probability_legit > probability_spam else 1, None



        # probability_classes = defaultdict(lambda: 0.0)
        # for key in self.classes_count.keys():
        #     probability_classes[key] = math.log(self.probability_class(key))
        #
        # for word in X_test:
        #     for key in probability_classes.keys():
        #         cur = math.log((self.frequencies.get((key, word), 0) + self.alpha) / (self.classes_count[key] + self.Q * self.alpha))
        #         if word in X_test:
        #             probability_classes[key] += cur
        #         else:
        #             probability_classes[key] += 1 - cur
        #
        # return max(probability_classes, key=probability_classes.get), probability_classes
