import argparse

import keras
import numpy as np
import pandas as pd
from keras.api import callbacks
from keras.api.layers import BatchNormalization, Dense, Dropout
from keras.api.models import Sequential

parser = argparse.ArgumentParser(
    description="Train a neural network to predict a category based on numerical features"
)
parser.add_argument("dataset", type=str, help="Path to the dataset csv file")
parser.add_argument("--category", type=str, help="Category to predict")
parser.add_argument("--weighted", "-w", action="store_true", help="Use class weights")
args = parser.parse_args()

FILE: str = args.dataset
CATEGORY: str = args.category
WEIGHTED: bool = args.weighted

df = pd.read_csv(FILE)
df[CATEGORY] = pd.Categorical(df[CATEGORY])

# Shuffle the dataset
df = df.sample(frac=1).reset_index(drop=True)

# X will be all the numerical features
X = df.select_dtypes(include=[np.number]).values
dummies = pd.get_dummies(df[CATEGORY])
Y = dummies.values
Y_names = dummies.columns

num_features = X.shape[1]
num_categories = Y.shape[1]

cat_sums = Y.sum(axis=0)
full_sum = cat_sums.sum()
class_weights = {}
for i in range(num_categories):
    if WEIGHTED:
        class_weights[i] = (full_sum - cat_sums[i]) / (cat_sums[i] * (num_categories - 1))
    else:
        class_weights[i] = 1

model = Sequential(name="BinaryPredictor")
model.add(Dense(num_features, input_dim=num_features, activation="sigmoid"))
model.add(BatchNormalization())
model.add(Dropout(0.3))
for _ in range(2):
    model.add(Dense(num_features, activation="sigmoid"))
    model.add(BatchNormalization())
    model.add(Dropout(0.3))
for _ in range(2):
    model.add(Dense(num_features, activation="leaky_relu"))
    model.add(BatchNormalization())
    model.add(Dropout(0.3))
for _ in range(num_categories + 2):
    model.add(Dense(num_features, activation="relu"))
    model.add(BatchNormalization())
    model.add(Dropout(0.3))
model.add(Dense(num_categories, activation="softmax"))
model.compile(optimizer="Nadam", loss="categorical_crossentropy", metrics=["accuracy"])
model.summary()


cbks = [callbacks.ReduceLROnPlateau(monitor="loss", factor=0.5, patience=10, min_lr=0.00000000001)]

try:
    model.fit(X, Y, epochs=2000, batch_size=64, callbacks=cbks, class_weight=class_weights)
except KeyboardInterrupt:
    print("Training interrupted")

# Create the contingency table (confusion matrix)
predictions = model.predict(X).argmax(axis=1)
conf_matrix = np.zeros((num_categories, num_categories), dtype=int)
for expe, pred in zip(Y, predictions):
    conf_matrix[expe, pred] += 1

confusion = pd.DataFrame(conf_matrix, columns=map(lambda s: f"Pred {s}", Y_names), index=Y_names)
print(confusion)

# Compute accuracy
accuracy = np.trace(conf_matrix) / np.sum(conf_matrix)
print(f"Accuracy: {accuracy*100:.2f}%")
