The COBOL code provided is a simple implementation of a perceptron, a type of artificial neuron used in machine learning for binary classification tasks. The code initializes weights and bias to random values, then iteratively updates them based on the error between the predicted and actual outputs.

Below is the equivalent Python code for the given COBOL program, along with unit tests using the `unittest` framework:

```python
# Author: Victor Ribeiro
# Date-Written: May 30th 2020

import random

class Perceptron:
    def __init__(self, learning_rate=0.001, iterations=10):
        self.lr = learning_rate
        self.it = iterations
        self.b = random.random()
        self.w1 = random.random()
        self.w2 = random.random()
        self.w3 = random.random()
        self.w4 = random.random()

    def predict(self, features):
        prediction = self.b
        prediction += self.w1 * features[0]
        prediction += self.w2 * features[1]
        prediction += self.w3 * features[2]
        prediction += self.w4 * features[3]
        return prediction

    def train(self, training_data):
        for _ in range(self.it):
            for data in training_data:
                features = data[:-1]
                y = data[-1]
                prediction = self.predict(features)
                err = y - prediction
                self.b += err * self.lr
                self.w1 += features[0] * err * self.lr
                self.w2 += features[1] * err * self.lr
                self.w3 += features[2] * err * self.lr
                self.w4 += features[3] * err * self.lr

    def classify(self, features):
        prediction = self.predict(features)
        return 1 if prediction > 0.5 else 0

# Example usage:
# perceptron = Perceptron()
# training_data = [
#     [0.1, 0.2, 0.3, 0.4, 0],
#     [0.5, 0.6, 0.7, 0.8, 1],
#     # ... more data ...
# ]
# perceptron.train(training_data)
# print(perceptron.classify([0.1, 0.2, 0.3, 0.4]))

# Unit tests
import unittest

class TestPerceptron(unittest.TestCase):
    def test_predict(self):
        p = Perceptron()
        p.b = 0
        p.w1 = 1
        p.w2 = 1
        p.w3 = 1
        p.w4 = 1
        self.assertEqual(p.predict([1, 1, 1, 1]), 4)

    def test_classify(self):
        p = Perceptron()
        p.b = -0.5
        p.w1 = 1
        p.w2 = 1
        p.w3 = 1
        p.w4 = 1
        self.assertEqual(p.classify([1, 1, 1, 1]), 1)
        self.assertEqual(p.classify([0, 0, 0, 0]), 0)

if __name__ == '__main__':
    unittest.main()
```

To run the unit tests, save the Python code and the unit tests in a file (e.g., `perceptron.py`) and then execute the file using the Python interpreter. The `unittest` framework will automatically run the test methods `test_predict` and `test_classify` and check if the predictions and classifications are as expected.