import pandas as pd
import matplotlib.pyplot as plt
data = pd.read_csv("C:\\Users\\Johnny\\Desktop\\DS\\Python files\\Decission tree\\iris.csv")
data.head()
data['Species'].unique()
data.Species.value_counts()
colnames = list(data.columns)
predictors = colnames[0:4]
target = colnames[4]

# Splitting data into training and testing data set

import numpy as np
from sklearn.model_selection import train_test_split
train,test = train_test_split(data,test_size = 0.2)

from sklearn.tree import  DecisionTreeClassifier
help(DecisionTreeClassifier)


model = DecisionTreeClassifier(criterion = 'entropy')
model.fit(train[predictors],train[target])

preds = model.predict(test[predictors])
type(preds)
pd.Series(preds).value_counts()

pd.crosstab(test[target],preds)


np.mean(preds==test.Species) # 0.933


