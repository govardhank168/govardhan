import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap
import seaborn as sns
from sklearn.preprocessing import normalize, LabelEncoder
from sklearn.neighbors import KNeighborsClassifier 
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix

# reading the data from csv
#glass = pd.read_csv('../input/glass.csv')
glass = pd.read_csv("D:\\DS\\Assignments\\Assignments\\KNN\\glass.csv")
glass.head()
glass.dtypes
glass.describe()

#converting the data to array for plotting. 
x = np.array(glass.iloc[:,0:10])
y = np.array(glass['Type'])
print("Shape of x:"+str(x.shape))
print("Shape of y:"+str(y.shape))

# custome color maps
cm_dark = ListedColormap(['#ff6060', '#8282ff','#ffaa00','#fff244','#76e8fc','#3ad628'])
cm_bright = ListedColormap(['#ffafaf', '#c6c6ff','#ffaa00','#ffe2a8','#bfffe7','#c9f7ff','#9eff93'])

# Create color maps
from matplotlib import pyplot as plt
plt.scatter(x[:,0],x[:,1],c=y,cmap=cm_dark,s=10,label=y)
plt.show()

# Try plots using seaborn
sns.swarmplot(x='Na',y='RI',data=glass,hue='Type')

# creating training set, test set and checking shapes 
from sklearn.model_selection import train_test_split
x_train,x_test,y_train,y_test = train_test_split(x,y)
print("Shape of x_train:"+str(x_train.shape))
print("Shape of y_train:"+str(y_train.shape))
print("Shape of x_test:"+str(x_test.shape))
print("Shape of y_test:"+str(y_test.shape))
print("Shape of y_test:"+str(y_test.reshape(1,-1)))

# Using KNN to classify the glasses 
knn = KNeighborsClassifier(n_neighbors=3)
knn.fit(x_train,y_train)

# Predicting results using KNN fit. 
pred = knn.predict(x_train)
pred

# Check accuracy
accuracy = knn.score(x_train,y_train)
print("The accuracy is :"+str(accuracy))

#computing confusion matrix 
cnf_matrix = confusion_matrix(y_train,pred)
print(cnf_matrix)

#plotting the matrix in with plotly
plt.imshow(cnf_matrix,cmap=plt.cm.jet)

#visualizing the confusion matrix
glass_cm = pd.DataFrame(cnf_matrix, range(6),range(6))
sns.set(font_scale=1.4)#for label size
sns.heatmap(glass_cm, annot=True,annot_kws={"size": 16})# font size

X = np.array(glass.drop(['Type'], 1))
y = np.array(glass['Type'])

from sklearn.model_selection import train_test_split
from sklearn.model_selection import cross_validate
from sklearn.neighbors import NearestNeighbors

x_train, x_test, y_train, y_test = train_test_split(x, y, test_size = 0.2) #20% data used

clf = KNeighborsClassifier()
clf.fit(x_train, y_train)
accuracy = clf.score(x_test, y_test) #test
print(accuracy) #this works fine

example = np.array([7,4,3,2,4,5,3,6,7,4,2,3,5,6,8,4])
example = example.reshape(1, -1)

prediction = clf.predict(x_test)
print(prediction)

def plot_decision_boundary(prediction):
    # Set min and max values and give it some padding
    x_min, x_max = x[:, 0].min() - .5, x[:, 0].max() + .5
    y_min, y_max = x[:, 1].min() - .5, x[:, 1].max() + .5
    h = 0.01
    # Generate a grid of points with distance h between them
    xx, yy = np.meshgrid(np.arange(x_min, x_max, h), np.arange(y_min, y_max, h))
    # Predict the function value for the whole gid
    
    prediction = prediction.var()
    prediction1 = prediction.var(axis = 1)
  
    Z = np.ravel([[xx], [yy]])
    Z = Z.reshape(xx.shape)
    # Plot the contour and training examples
    plt.contourf(xx, yy, Z, cmap=plt.cm.Spectral)
    plt.scatter(x[:, 0], x[:, 1], c=y, cmap=plt.cm.Spectral)


import numpy as np
import flask
print(flask)
list1 = [1.0,2.0,3.0,4.0,5.0]
npy_array = np.array(list1, dtype = np.float64)
npy_array.ndim
npy_array.shape
 
plt.figure()
from sklearn.naive_bayes import GaussianNB

clf.fit(x_train, y_train)
plt.title("Naive Bayes decision boundary", fontsize = 3)
plot_decision_boundary(x_train, y_train,GaussianNB)
plt.show()

from sklearn.linear_model import LogisticRegression
plt.figure()
plt.title("Logistic regression decision boundary", fontsize = 16)
plot_decision_boundary(x_train, y_train, LogisticRegression)
plt.show()

plt.figure()
plt.title("K-nearest neighbor decision boundary", fontsize = 16)
plot_decision_boundary(x_train, y_train, KNeighborsClassifier)
plt.show()

from sklearn.neural_network import MLPClassifier
plot_decision_boundary(x_test,y_test, MLPClassifier, hidden_layer_sizes=(5,2), solver='sgd', learning_rate_init=0.001,max_iter=1000)
plt.show()

plt.figure()
plt.title("KNN decision boundary with neighbors: {}".format(type), fontsize = 16)
plot_decision_boundary(x_train, y_train, KNeighborsClassifier, n_neighbors=4)
plt.show()

#---------------------------
h = .08  # step size in the mesh
n_neighbors = 5 # No of neighbours
for weights in ['uniform', 'distance']:
    # we create an instance of Neighbours Classifier and fit the data.
    clf = KNeighborsClassifier(n_neighbors, weights=weights)
    clf.fit(x, y)

    # Plot the decision boundary. For that, we will assign a color to each
    # point in the mesh [x_min, x_max]x[y_min, y_max].
    x_min, x_max = x[:, 0].min() - 1, x[:, 0].max() + 1
    y_min, y_max = x[:, 1].min() - 1, x[:, 1].max() + 1
    xx, yy = np.meshgrid(np.arange(x_min, x_max, h), np.arange(y_min, y_max, h))
    
    Z = clf.predict(np.c_[xx.ravel(), yy.ravel()])   # ravel to flatten the into 1D and c_ to concatenate 

    # Put the result into a color plot
    Z = Z.reshape(xx.shape)
    plt.figure()
    plt.pcolormesh(xx, yy, Z, cmap=cm_bright)

    # Plot also the training points
    plt.scatter(x[:, 0], x[:, 1], c=y, cmap=cm_dark, edgecolor='k', s=20)
    plt.xlim(xx.min(), xx.max())
    plt.ylim(yy.min(), yy.max())
    plt.title("3-Class classification (k = %i, weights = '%s')"% (n_neighbors, weights))

plt.show()

#Gradaint decent--------------------------------------------------
import numpy as np

class GradientDescentLinearRegression:
    def __init__(self, learning_rate=0.01, iterations=1000):
        self.learning_rate, self.iterations = learning_rate, iterations
    
    def fit(self, x, y):
        b = 0
        m = 5
        n = x.shape[0]
        for _ in range(self.iterations):
            b_gradient = -2 * np.sum(y - m*x + b) / n
            m_gradient = -2 * np.sum(X*(y - (m*x + b))) / n
            b = b + (self.learning_rate * b_gradient)
            m = m - (self.learning_rate * m_gradient)
        self.m, self.b = m, b
        
    def predict(self, x):
        return self.m*x + self.b

np.random.seed(42)
x = np.array(sorted(list(range(5))*20)) + np.random.normal(size=100, scale=0.5)
y = np.array(sorted(list(range(5))*20)) + np.random.normal(size=100, scale=0.25)
y=y.reshape(1,-1)
clf = GradientDescentLinearRegression()
clf.fit(x, y)

import matplotlib.pyplot as plt
plt.style.use('fivethirtyeight')

plt.scatter(x, y, color='green')
import pandas as pd
import scipy.stats as stats
import matplotlib.pyplot as plt
import sklearn
from sklearn.datasets import load_boston
from sklearn.linear_model import LinearRegression
plt.plot(x, clf.predict(x))
plt.gca().set_title("Gradient Descent Linear Regressor")

