import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.graphics.tsaplots as tsa_plots
from statsmodels.tsa.arima_model import ARIMA

Walmart = pd.read_csv("E:\Day Wise\Day 40 Forecasting cont\Data\Walmart Footfalls Raw.csv")
Train = Walmart.head(147)
Test = Walmart.tail(12)

tsa_plots.plot_acf(Walmart.Footfalls,lags=12)
tsa_plots.plot_pacf(Walmart.Footfalls,lags=12)


model1=ARIMA(Walmart.Footfalls,order=(12,1,1)).fit(disp=0)
model2=ARIMA(Walmart.Footfalls,order=(1,1,5)).fit(disp=0)
model1.aic
model2.aic

p=1
q=0
d=1
pdq=[]
aic=[]
for q in range(7):
    model=ARIMA(Walmart.Footfalls,order=(p,d,q)).fit(disp=0)
    x = model.aic
    x1 = p,d,q

    aic.append(x)
    pdq.append(x1)

keys = pdq
values = aic
d = dict(zip(keys, values))
print (d)

from pandas.plotting import autocorrelation_plot
autocorrelation_plot(Walmart.Footfalls)