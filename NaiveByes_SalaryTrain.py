#Importing the libraries
import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
import matplotlib.pyplot as plt # for data visualization purposes
import seaborn as sns # for statistical data visualization

#%matplotlib inline

from sklearn.model_selection import train_test_split as tts


#Importing the CSV File
Salary_train = pd.read_csv("D:\\DS\\Assignments\\Assignments\\Naive Bayes\\SalaryData_Train.csv")

#printing the Dataset
Salary_train

#X is the matrix of features, it contains independent variable number 2 and 3 which is Age,EstimatedSalary according to dataset
X1 = Salary_train.iloc[:,[1,12]].values
#Y contains dependent variable which is Purchased according to dataset and the column number is 4
Y1 = Salary_train.iloc[:,13].values

#printing the independent variable matrix
X1

#printing the dependent variable vector
Y1

#Importing the CSV File
Salary_test = pd.read_csv("D:\\DS\\Assignments\\Assignments\\Naive Bayes\\SalaryData_Test.csv")

#printing the Dataset
Salary_test

#X is the matrix of features, it contains independent variable number 2 and 3 which is Age,EstimatedSalary according to dataset
X2 = Salary_test.iloc[:,[1,12]].values
#Y contains dependent variable which is Purchased according to dataset and the column number is 4
Y2 = Salary_test.iloc[:,13].values

#printing the independent variable matrix
X2

#printing the dependent variable vector
Y2

from gtts import gTTS
blabla = ("Spoken text")
tts = gTTS(text=blabla, lang='en')
tts.save("C:/test.mp3")
# Splitting the dataset into the Training set and Test set
from sklearn.model_selection import train_test_split as sklearn_train_test_split
X_train, X_test, y_train, y_test = gTTS(X1,Y1,test_size=0.25,random_state=30)

# Predicting the Test set results
import keras
from keras.models import Sequential
from keras.layers import Dense
from keras.models import load_model
#empty model
classifier = Sequential()
#add layers, start with hidden layer and first deep layer
classifier.add(Dense(output_dim=15, init="uniform", activation='relu',input_dim = 15))
classifier.add(Dropout(rate=0.1))
prediction = classifier.predict(X_test)

#Printing the predictions
prediction

# Making the Confusion Matrix
from sklearn.metrics import confusion_matrix
confusion_matrix = confusion_matrix(y_test, prediction)

#print the matrix
confusion_matrix
# so here 55+18=73 predictions are correct, which is really good out of 80

#calculating the accuracy of this model w.r.t. this dataset
from sklearn.metrics import accuracy_score
print(accuracy_score(y_test, prediction))

#Visualising the Training set results
from matplotlib.colors import ListedColormap
X_set, y_set = X_train, y_train
X1, X2 = np.meshgrid(np.arange(start = X_set[:, 0].min() - 1, stop = X_set[:, 0].max() + 1, step = 0.01),
                     np.arange(start = X_set[:, 1].min() - 1, stop = X_set[:, 1].max() + 1, step = 0.01))
plt.contourf(X1, X2, classifier.predict(np.array([X1.ravel(), X2.ravel()]).T).reshape(X1.shape),
             alpha = 0.75, cmap = ListedColormap(('red', 'green')))
plt.xlim(X1.min(), X1.max())
plt.ylim(X2.min(), X2.max())
for i, j in enumerate(np.unique(y_set)):
    plt.scatter(X_set[y_set == j, 0], X_set[y_set == j, 1],
                c = ListedColormap(('red', 'green'))(i), label = j)
plt.title('Naive Bayes (Training set)')
plt.xlabel('Age')
plt.ylabel('Estimated Salary')
plt.legend()
plt.show()

#Visualising the Test set results
from matplotlib.colors import ListedColormap
X_set, y_set = X_test, y_test
X1, X2 = np.meshgrid(np.arange(start = X_set[:, 0].min() - 1, stop = X_set[:, 0].max() + 1, step = 0.01),
                     np.arange(start = X_set[:, 1].min() - 1, stop = X_set[:, 1].max() + 1, step = 0.01))
plt.contourf(X1, X2, classifier.predict(np.array([X1.ravel(), X2.ravel()]).T).reshape(X1.shape),
             alpha = 0.75, cmap = ListedColormap(('red', 'green')))
plt.xlim(X1.min(), X1.max())
plt.ylim(X2.min(), X2.max())
for i, j in enumerate(np.unique(y_set)):
    plt.scatter(X_set[y_set == j, 0], X_set[y_set == j, 1],
                c = ListedColormap(('red', 'green'))(i), label = j)
plt.title('Naive Bayes (Test set)')
plt.xlabel('Age')
plt.ylabel('Estimated Salary')
plt.legend()
plt.show()


#----------------------------------------------------
# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list all files under the input directory

import os
for dirname, _, filenames in os.walk('/kaggle/input'):
    for filename in filenames:
        print(os.path.join(dirname, filename))

# Any results you write to the current directory are saved as output.
import warnings
warnings.filterwarnings('ignore')


#1.Import dataset 
data = '/kaggle/input/adult-dataset/adult.csv'
df = pd.read_csv(data, header=None, sep=',\s')

#2.Exploratory data analysis 
   #view dimensions of dataset

df.shape
# preview the dataset
df.head()

col_names = ['age', 'workclass', 'fnlwgt', 'education', 'education_num', 'marital_status', 'occupation', 'relationship',
             'race', 'sex', 'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income']

df.columns = col_names
df.columns

# let's again preview the dataset
df.head()

# view summary of dataset
df.info()

#Explore categorical variables
# find categorical variables
categorical = [var for var in df.columns if df[var].dtype=='O']
print('There are {} categorical variables\n'.format(len(categorical)))
print('The categorical variables are :\n\n', categorical)

# view the categorical variables
df[categorical].head()

#Missing values in categorical variables
# check missing values in categorical variables
df[categorical].isnull().sum()

#Frequency counts of categorical variables
#Now, I will check the frequency counts of categorical variables.

# view frequency counts of values in categorical variables
for var in categorical: 
        print(df[var].value_counts())

#Explore workclass variable
# check labels in workclass variable
df.workclass.unique()

# check frequency distribution of values in workclass variable
df.workclass.value_counts()

#We can see that there are 1836 values encoded as ? in workclass variable. I will replace these ? with NaN.
# replace '?' values in workclass variable with `NaN`
df['workclass'].replace('?', np.NaN, inplace=True)

# again check the frequency distribution of values in workclass variable
df.workclass.value_counts()

#Explore occupation variable
# check labels in occupation variable
df.occupation.unique()

# check frequency distribution of values in occupation variable
df.occupation.value_counts()

# replace '?' values in occupation variable with `NaN`
df['occupation'].replace('?', np.NaN, inplace=True)

# again check the frequency distribution of values in occupation variable
df.occupation.value_counts()

# check labels in native_country variable
df.native_country.unique()

# check frequency distribution of values in native_country variable
df.native_country.value_counts()

# replace '?' values in native_country variable with `NaN`
df['native_country'].replace('?', np.NaN, inplace=True)

# again check the frequency distribution of values in native_country variable
df.native_country.value_counts()

#Check missing values in categorical variables again
df[categorical].isnull().sum()

# check for cardinality in categorical variables

for var in categorical:
      print(var, ' contains ', len(df[var].unique()), ' labels')

# find numerical variables

numerical = [var for var in df.columns if df[var].dtype!='O']

print('There are {} numerical variables\n'.format(len(numerical)))
print('The numerical variables are :', numerical)


# view the numerical variables
df[numerical].head()

#Missing values in numerical variables
# check missing values in numerical variables
df[numerical].isnull().sum()

#3. Declare feature vector and target variable 
X = df.drop(['income'], axis=1)
y = df['income']

#4. Split data into separate training and test set 
# split X and y into training and testing sets
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.3, random_state = 0)

# check the shape of X_train and X_test
X_train.shape, X_test.shape

#5.Feature Engineering 
# can display the categorical and numerical variables again separately.
# check data types in X_train
X_train.dtypes

# display categorical variables
categorical = [col for col in X_train.columns if X_train[col].dtypes == 'O']
categorical

# display numerical variables
numerical = [col for col in X_train.columns if X_train[col].dtypes != 'O']
numerical

# print percentage of missing values in the categorical variables in training set
X_train[categorical].isnull().mean()

# print categorical variables with missing data
for col in categorical:
    if X_train[col].isnull().mean()>0:
        print(col, (X_train[col].isnull().mean()))

# impute missing categorical variables with most frequent value
for df2 in [X_train, X_test]:
    df2['workclass'].fillna(X_train['workclass'].mode()[0], inplace=True)
    df2['occupation'].fillna(X_train['occupation'].mode()[0], inplace=True)
    df2['native_country'].fillna(X_train['native_country'].mode()[0], inplace=True)    

# check missing values in categorical variables in X_train
X_train[categorical].isnull().sum()

# check missing values in categorical variables in X_test
X_test[categorical].isnull().sum()

#can check for missing values in X_train and X_test.
#check missing values in X_train
X_train.isnull().sum()

# check missing values in X_test
X_test.isnull().sum()

# print categorical variables
categorical

X_train[categorical].head()

# import category encoders
import category_encoders as ce

# encode remaining variables with one-hot encoding
encoder = ce.OneHotEncoder(cols=['workclass', 'education', 'marital_status', 'occupation', 'relationship', 
                                 'race', 'sex', 'native_country'])

X_train = encoder.fit_transform(X_train)
X_test = encoder.transform(X_test)
X_train.head()

X_train.shape
X_test.head()

X_test.shape

cols = X_train.columns

from sklearn.preprocessing import RobustScaler
scaler = RobustScaler()
X_train = scaler.fit_transform(X_train)
X_test = scaler.transform(X_test)
X_train = pd.DataFrame(X_train, columns=[cols])
X_test = pd.DataFrame(X_test, columns=[cols])
X_train.head()

#6.Model training 
# train a Gaussian Naive Bayes classifier on the training set
from sklearn.naive_bayes import GaussianNB
# instantiate the model
gnb = GaussianNB()

# fit the model
gnb.fit(X_train, y_train)
GaussianNB(priors=None, var_smoothing=1e-09)
#7.Predict the results 
y_pred = gnb.predict(X_test)
y_pred
array(['<=50K', '<=50K', '>50K', ..., '>50K', '<=50K', '<=50K'],
      dtype='<U5')
#7.Check accuracy score 
from sklearn.metrics import accuracy_score
print('Model accuracy score: {0:0.4f}'. format(accuracy_score(y_test, y_pred)))
y_pred_train = gnb.predict(X_train)
y_pred_train

print('Training-set accuracy score: {0:0.4f}'. format(accuracy_score(y_train, y_pred_train)))

# print the scores on training and test set
print('Training set score: {:.4f}'.format(gnb.score(X_train, y_train)))
print('Test set score: {:.4f}'.format(gnb.score(X_test, y_test)))

# check class distribution in test set
y_test.value_counts()

# check null accuracy score
null_accuracy = (7407/(7407+2362))

print('Null accuracy score: {0:0.4f}'. format(null_accuracy))

#8. Confusion matrix 
# Print the Confusion Matrix and slice it into four pieces
from sklearn.metrics import confusion_matrix
cm = confusion_matrix(y_test, y_pred)
print('Confusion matrix\n\n', cm)
print('\nTrue Positives(TP) = ', cm[0,0])
print('\nTrue Negatives(TN) = ', cm[1,1])
print('\nFalse Positives(FP) = ', cm[0,1])
print('\nFalse Negatives(FN) = ', cm[1,0])

# visualize confusion matrix with seaborn heatmap
cm_matrix = pd.DataFrame(data=cm, columns=['Actual Positive:1', 'Actual Negative:0'], 
                                 index=['Predict Positive:1', 'Predict Negative:0'])

sns.heatmap(cm_matrix, annot=True, fmt='d', cmap='YlGnBu')

#9.Classification metrices 
from sklearn.metrics import classification_report
print(classification_report(y_test, y_pred))

TP = cm[0,0]
TN = cm[1,1]
FP = cm[0,1]
FN = cm[1,0]

# print classification accuracy
classification_accuracy = (TP + TN) / float(TP + TN + FP + FN)
print('Classification accuracy : {0:0.4f}'.format(classification_accuracy))

# print classification error
classification_error = (FP + FN) / float(TP + TN + FP + FN)
print('Classification error : {0:0.4f}'.format(classification_error))

Precision
#Precision can be defined as the percentage of correctly predicted positive outcomes out of all the predicted positive outcomes
#Mathematically, precision can be defined as the ratio of TP to (TP + FP).
# print precision score
precision = TP / float(TP + FP)
print('Precision : {0:0.4f}'.format(precision))

recall = TP / float(TP + FN)
print('Recall or Sensitivity : {0:0.4f}'.format(recall))

true_positive_rate = TP / float(TP + FN)

print('True Positive Rate : {0:0.4f}'.format(true_positive_rate))

false_positive_rate = FP / float(FP + TN)

print('False Positive Rate : {0:0.4f}'.format(false_positive_rate))

specificity = TN / (TN + FP)

print('Specificity : {0:0.4f}'.format(specificity))

#10. Calculate class probabilities 
# print the first 10 predicted probabilities of two classes- 0 and 1
y_pred_prob = gnb.predict_proba(X_test)[0:10]
y_pred_prob

Observations
•	In each row, the numbers sum to 1.
•	There are 2 columns which correspond to 2 classes - <=50K and >50K.
?	Class 0 => <=50K - Class that a person makes less than equal to 50K.
?	Class 1 => >50K - Class that a person makes more than 50K.
•	Importance of predicted probabilities
?	We can rank the observations by probability of whether a person makes less than or equal to 50K or more than 50K.
•	predict_proba process
?	Predicts the probabilities
?	Choose the class with the highest probability
•	Classification threshold level
?	There is a classification threshold level of 0.5.
?	Class 0 => <=50K - probability of salary less than or equal to 50K is predicted if probability < 0.5.
?	Class 1 => >50K - probability of salary more than 50K is predicted if probability > 0.5.

# store the probabilities in dataframe
y_pred_prob_df = pd.DataFrame(data=y_pred_prob, columns=['Prob of - <=50K', 'Prob of - >50K'])
y_pred_prob_df

# print the first 10 predicted probabilities for class 1 - Probability of >50K
gnb.predict_proba(X_test)[0:10, 1]


# store the predicted probabilities for class 1 - Probability of >50K
y_pred1 = gnb.predict_proba(X_test)[:, 1]

# plot histogram of predicted probabilities
# adjust the font size 
plt.rcParams['font.size'] = 12

# plot histogram with 10 bins
plt.hist(y_pred1, bins = 10)

# set the title of predicted probabilities
plt.title('Histogram of predicted probabilities of salaries >50K')

# set the x-axis limit
plt.xlim(0,1)

# set the title
plt.xlabel('Predicted probabilities of salaries >50K')
plt.ylabel('Frequency')
Text(0, 0.5, 'Frequency')

Observations
•	We can see that the above histogram is highly positive skewed.
•	The first column tell us that there are approximately 5700 observations with probability between 0.0 and 0.1 whose salary is <=50K.
•	There are relatively small number of observations with probability > 0.5.
•	So, these small number of observations predict that the salaries will be >50K.
•	Majority of observations predcit that the salaries will be <=50K.
#11. ROC - AUC 
# plot ROC Curve
from sklearn.metrics import roc_curve
fpr, tpr, thresholds = roc_curve(y_test, y_pred1, pos_label = '>50K')
plt.figure(figsize=(6,4))
plt.plot(fpr, tpr, linewidth=2)
plt.plot([0,1], [0,1], 'k--' )
plt.rcParams['font.size'] = 12
plt.title('ROC curve for Gaussian Naive Bayes Classifier for Predicting Salaries')
plt.xlabel('False Positive Rate (1 - Specificity)')
plt.ylabel('True Positive Rate (Sensitivity)')
plt.show()

ROC curve help us to choose a threshold level that balances sensitivity and specificity for a particular context.
ROC AUC
ROC AUC stands for Receiver Operating Characteristic - Area Under Curve. It is a technique to compare classifier performance. In this technique, we measure the area under the curve (AUC). A perfect classifier will have a ROC AUC equal to 1, whereas a purely random classifier will have a ROC AUC equal to 0.5.
So, ROC AUC is the percentage of the ROC plot that is underneath the curve.

# compute ROC AUC

from sklearn.metrics import roc_auc_score

ROC_AUC = roc_auc_score(y_test, y_pred1)

print('ROC AUC : {:.4f}'.format(ROC_AUC))

Interpretation
•	ROC AUC is a single number summary of classifier performance. The higher the value, the better the classifier.
•	ROC AUC of our model approaches towards 1. So, we can conclude that our classifier does a good job in predicting whether it will rain tomorrow or not.

# calculate cross-validated ROC AUC 
from sklearn.model_selection import cross_val_score
Cross_validated_ROC_AUC = cross_val_score(gnb, X_train, y_train, cv=5, scoring='roc_auc').mean()
print('Cross validated ROC AUC : {:.4f}'.format(Cross_validated_ROC_AUC))

#12.k-Fold Cross Validation 
# Applying 10-Fold Cross Validation
from sklearn.model_selection import cross_val_score
scores = cross_val_score(gnb, X_train, y_train, cv = 10, scoring='accuracy')
print('Cross-validation scores:{}'.format(scores))

# compute Average cross-validation score
print('Average cross-validation score: {:.4f}'.format(scores.mean()))

#13. Results and conclusion 
Table of Contents
1.	In this project, I build a Gaussian Naïve Bayes Classifier model to predict whether a person makes over 50K a year. The model yields a very good performance as indicated by the model accuracy which was found to be 0.8083.
2.	The training-set accuracy score is 0.8067 while the test-set accuracy to be 0.8083. These two values are quite comparable. So, there is no sign of overfitting.
3.	I have compared the model accuracy score which is 0.8083 with null accuracy score which is 0.7582. So, we can conclude that our Gaussian Naïve Bayes classifier model is doing a very good job in predicting the class labels.
4.	ROC AUC of our model approaches towards 1. So, we can conclude that our classifier does a very good job in predicting whether a person makes over 50K a year.
5.	Using the mean cross-validation, we can conclude that we expect the model to be around 80.63% accurate on average.
6.	If we look at all the 10 scores produced by the 10-fold cross-validation, we can also conclude that there is a relatively small variance in the accuracy between folds, ranging from 81.35% accuracy to 79.64% accuracy. So, we can conclude that the model is independent of the particular folds used for training.
7.	Our original model accuracy is 0.8083, but the mean cross-validation accuracy is 0.8063. So, the 10-fold cross-validation accuracy does not result in performance improvement for this model.

