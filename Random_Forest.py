import pandas as pd
import numpy as np
# Reading the Diabetes Data #################
Diabetes = pd.read_csv("C:\\Users\\Johnny\\Desktop\\DS\\Python files\\Random Forest\\Diabetes.csv")
Diabetes.head()
Diabetes.columns
colnames = list(Diabetes.columns)
predictors = colnames[:8]
target = colnames[8]

X = Diabetes[predictors]
Y = Diabetes[target]



####### GridSearch 

from sklearn.ensemble import RandomForestClassifier
rf = RandomForestClassifier(n_jobs=2,oob_score=True,n_estimators=1000,criterion="entropy")

# n_estimators -> Number of trees ( you can increase for better accuracy)
# n_jobs -> Parallelization of the computing and signifies the number of jobs 
# running parallel for both fit and predict
# oob_score = True means model has done out of box sampling to make predictions

np.shape(Diabetes) # 768,9 => Shape 

#### Attributes that comes along with RandomForest function
rf.fit(X,Y) # Fitting RandomForestClassifier model from sklearn.ensemble 
rf.estimators_ # 
rf.classes_ # class labels (output)
rf.n_classes_ # Number of levels in class labels 
rf.n_features_  # Number of input features in model 8 here.

rf.n_outputs_ # Number of outputs when fit performed

rf.predict(X)
##############################

Diabetes['rf_pred'] = rf.predict(X)

from sklearn.metrics import confusion_matrix
confusion_matrix(Diabetes['Outcome'],Diabetes['rf_pred']) # Confusion matrix

pd.crosstab(Diabetes['Outcome'],Diabetes['rf_pred'])



print("Accuracy",(500+268)/(500+268)*100)

# Accuracy is 99.609375
Diabetes["rf_pred"]

######################### IRIS data set #############
iris = pd.read_csv("E:\\Bokey\\Excelr Data\\Python Codes\\all_py\\KNN\\iris.csv")
iX=iris[["Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"]]
iy=iris[["Species"]]

from sklearn.ensemble import RandomForestClassifier
rfiris = RandomForestClassifier(n_jobs=4,oob_score=True,n_estimators=100,criterion="entropy")
rfiris.fit(iX,iy)
iris["rf_pred"] = rfiris.predict(iX)

from sklearn.metrics import confusion_matrix
confusion_matrix(iris["Species"],iris["rf_pred"]) # 100 Percent 
pd.crosstab(iris["Species"],iris["rf_pred"])


# We need to split data into training and testing and again we need to perform Random Forests

