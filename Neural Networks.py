
# Reading data 
import pandas as pd
import numpy as np

# Importing necessary models for implementation of ANN
from keras.models import Sequential
from keras.layers import Dense #, Activation,Layer,Lambda

Concrete = pd.read_csv("C:\\Users\\Johnny\\Desktop\\DS\\Python files\\Neural networks\\concrete.csv")
Concrete.head()

#def prep_model(hidden_dim):
#    model = Sequential()
#    for i in range(1,len(hidden_dim)-1):
#        if (i==1):
#            model.add(Dense(hidden_dim[i],input_dim=hidden_dim[0],kernel_initializer="normal",activation="relu"))
#        else:
#            model.add(Dense(hidden_dim[i],activation="relu"))
#    # for the output layer we are not adding any activation function as 
#    # the target variable is continuous variable 
#    model.add(Dense(hidden_dim[-1]))
#    # loss ---> loss function is means squared error to compare the output and estimated output
#    # optimizer ---> adam
#    # metrics ----> mean squared error - error for each epoch on entire data set 
#    model.compile(loss="mean_squared_error",optimizer="adam",metrics = ["mse"])
#   return (model)


cont_model = Sequential()
cont_model.add(Dense(50,input_dim=8,activation="relu"))
cont_model.add(Dense(40,activation="relu"))
cont_model.add(Dense(20,activation="relu"))
cont_model.add(Dense(1,kernel_initializer="normal"))
cont_model.compile(loss="mean_squared_error",optimizer = "adam",metrics = ["mse"])

column_names = list(Concrete.columns)
predictors = column_names[0:8]
target = column_names[8]

#first_model = prep_model([8,50,1])
first_model = cont_model
first_model.fit(np.array(Concrete[predictors]),np.array(Concrete[target]),epochs=10)
pred_train = first_model.predict(np.array(Concrete[predictors]))
pred_train = pd.Series([i[0] for i in pred_train])
rmse_value = np.sqrt(np.mean((pred_train-Concrete[target])**2))

import matplotlib.pyplot as plt
plt.plot(pred_train,Concrete[target],"bo")
np.corrcoef(pred_train,Concrete[target]) # we got high correlation 

from ann_visualizer.visualize import ann_viz

ann_viz(first_model, title="My first neural network")
