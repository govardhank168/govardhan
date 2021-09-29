import pandas as pd
import numpy as np
from datetime import date
import seaborn as sns
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from sklearn.impute import SimpleImputer
from statsmodels.tools.eval_measures import rmse
from pmdarima import auto_arima
import statsmodels.api as sm
sm.tsa.statespace.SARIMAX
# for warnings 
import warnings
warnings.filterwarnings('ignore')
# Import the data
data = pd.read_excel('E:\\Pollution Forecasting\\Delhi (1).xlsx')
data.describe()
data.head()
data.tail()
data.shape
data.nunique()

# summarize the dataset
print(data.describe())
data.drop(index=0, inplace=True)
data.drop(columns=['Unnamed: 2'], inplace=True)
data.columns = data.iloc[0]
data = data.reindex(data.index.drop(1)).reset_index(drop=True)
data.columns.name = None
print(data.describe())
print(data['pm25'].describe())
data['pm25'] = data['pm25'].replace('-', np.NaN)
plt.figure(figsize=(9, 8))
sns.distplot(data['pm25'], color='blue', bins=100, hist_kws={'alpha': 0.4})
#avoid having the matplotlib verbose informations
data.hist(figsize=(16, 20), bins=50, xlabelsize=8, ylabelsize=8)
#to check missing value
sns.heatmap(data.isnull(), cbar=True, yticklabels=False, cmap='viridis')

#data groups through their quartiles
x = "pm25"
fig, ax = plt.subplots(ncols=2,  sharex=False, sharey=False)
fig.suptitle(x, fontsize=20)### distribution
ax[0].title.set_text('distribution')
variable = data[x].fillna(data[x].mean())
breaks = np.quantile(variable, q=np.linspace(0, 1, 11))
variable = variable[ (variable > breaks[0]) & (variable < breaks[10]) ]
sns.distplot(variable, hist=True, kde=True, kde_kws={"shade": True}, ax=ax[0])
des = data[x].describe()
ax[0].axvline(des["25%"], ls='--')
ax[0].axvline(des["mean"], ls='--')
ax[0].axvline(des["75%"], ls='--')
ax[0].grid(True)
des = round(des, 2).apply(lambda x: str(x))
box = '\n'.join(("min: "+des["min"], "25%: "+des["25%"], "mean: "+des["mean"], "75%: "+des["75%"], "max: "+des["max"]))
ax[0].text(0.95, 0.95, box, transform=ax[0].transAxes, fontsize=10, va='top', ha="right", bbox=dict(boxstyle='round', facecolor='white', alpha=1))### boxplot 
ax[1].title.set_text('outliers (log scale)')
tmp_data = pd.DataFrame(data[x])
tmp_data[x] = np.log(tmp_data[x])
tmp_data.boxplot(column=x, ax=ax[1])
plt.show()
#factorplot
sns.factorplot('pm25', data=data, kind='count')

#Bi-Variate Analysis
corr=data.corr(method='spearman')
plt.figure(figsize=(15,15))
sns.heatmap(corr,vmax=.8, linewidths=0.01, square=True, annot=True, cmap='RdBu', linecolor='black')
#ax = sns.heatmap(corr, vmin=-1, vmax=1, center=0, cmap=sns.diverging_palette(20, 220, n=200), square=True)
#ax.set_xticklabels(ax.get_xticklabels(), rotation=45, horizontalalignment='right')
#plt.show()
plt.figure(figsize=(12, 8))
sns.heatmap(corr, xticklabels = corr.columns.values, yticklabels = corr.columns.values, annot = True)
plt.show()

ts=data.set_index('date')
ts = ts.fillna(method='ffill')
imputer = SimpleImputer(missing_values=np.NaN, strategy='mean')
#Check Stationarity of a Time Series
fig, ax = plt.subplots(figsize=(8,12))
ax.plot(ts, color='blue')
ax.xaxis.set_major_formatter(mdates.DateFormatter('%m-%d'))
ax.set(xlabel="date", ylabel="pm25", title="Air pollution data in particulate matter - 2018")
plt.show()
ts = ts.sort_values('date')

from statsmodels.tsa.stattools import adfuller
def test_stationarity(timeseries):
    rolmean = ts.iloc[:,0].rolling(window=12).mean()
    rolstd = ts.iloc[:,0].rolling(window=12).std()
    
    #Plot rolling statistics:
    orig = plt.plot(ts['pm25'], color='blue',label='Original')
    mean = plt.plot(rolmean, color='red', label='Rolling Mean')
    std = plt.plot(rolstd, color='black', label = 'Rolling Std')
    plt.legend(loc='best')
    plt.title('Rolling Mean & Standard Deviation')
    plt.show(block=False)
    
    #Perform Dickey-Fuller test:
    print ('Results of Dickey-Fuller Test: 2018')
    dftest = adfuller(ts['pm25'], autolag='AIC')
    dfoutput = pd.Series(dftest[0:4], index=['Test Statistic','p-value','#Lags Used','Number of Observations Used'])
    for key,value in dftest[4].items():
        dfoutput['Critical Value (%s)'%key] = value
    print (dfoutput)

test_stationarity(ts)


#How to make a Time Series Stationary 1.Trend 2.Seasonality
#Estimating & Eliminating Trend
ts_log = np.log(ts)
plt.plot(ts_log)

#Moving average
moving_avg = ts_log.rolling(12).mean()
plt.plot(ts_log)
plt.plot(moving_avg, color='red')

ts_log_moving_avg_diff = ts_log - moving_avg
ts_log_moving_avg_diff.head(20)
ts_log_moving_avg_diff.dropna(inplace=True)
test_stationarity(ts_log_moving_avg_diff)

exp_wighted_avg = ts_log.ewm(halflife=12,min_periods=0,adjust=True,ignore_na=False).mean()
plt.plot(ts_log)
plt.plot(exp_wighted_avg, color='red')

ts_log_ewma_diff = ts_log - exp_wighted_avg
test_stationarity(ts_log_ewma_diff)

#Eliminating Trend and Seasonality 1.Differencing 2.Decomposing
#Differencing
ts_log_diff = ts_log_ewma_diff - ts_log.shift()
plt.plot(ts_log_diff)

ts_log.dropna(inplace=True)
test_stationarity(ts_log)
#Decomposing
from statsmodels.tsa.seasonal import seasonal_decompose
ts_log['pm25'] = ts_log['pm25'].astype(object)
decomposition = seasonal_decompose(ts_log, model = 'additive', period = int(len(ts_log)/2))
trend = decomposition.trend
seasonal = decomposition.seasonal
residual = decomposition.resid

plt.subplot(411)
plt.plot(ts_log, label='Original',color='green')
plt.legend(loc='best')
plt.subplot(412)
plt.plot(trend, label='Trend', color='orange')
plt.legend(loc='best')
plt.subplot(413)
plt.plot(seasonal,label='Seasonality', color='blue')
plt.legend(loc='best')
plt.subplot(414)
plt.plot(residual, label='Residuals', color='maroon')
plt.legend(loc='best')
plt.tight_layout()
plt.show()

ts_log_decompose = residual
ts_log_decompose.dropna(inplace=True)
test_stationarity(ts_log_decompose)

#Forecasting a Time Series
#ACF and PACF plots:
from statsmodels.tsa.stattools import acf, pacf
lag_acf = acf(ts_log, nlags=20)
lag_pacf = pacf(ts_log, nlags=20, method='ols')

#Plot ACF: 
plt.subplot(121) 
plt.plot(lag_acf)
plt.axhline(y=0,linestyle='--',color='red')
plt.axhline(y=-1.96/np.sqrt(len(ts_log)),linestyle='--',color='red')
plt.axhline(y=1.96/np.sqrt(len(ts_log)),linestyle='--',color='red')
plt.title('Autocorrelation Function')

#Plot PACF:
plt.subplot(122)
plt.plot(lag_pacf)
plt.axhline(y=0,linestyle='--',color='red')
plt.axhline(y=-1.96/np.sqrt(len(ts_log)),linestyle='--',color='red')
plt.axhline(y=1.96/np.sqrt(len(ts_log)),linestyle='--',color='red')
plt.title('Partial Autocorrelation Function')
plt.tight_layout()

# Fit auto_arima function to pollution ParticulateMattar dataset 
#stepwise_fit = auto_arima(ts_log['pm25'], start_p = 1, start_q = 1, 
#                          max_p = 3, max_q = 3, m = 12, 
 #                         start_P = 0, seasonal = True, 
  #                        d = None, D = 1, trace = True, 
   #                       error_action ='ignore',   # we don't want to know if an order does not work 
    #                      suppress_warnings = True,  # we don't want convergence warnings 
     #                     stepwise = True)           # set to stepwise 
  
# To print the summary 
#stepwise_fit.summary()

#AR Model
from statsmodels.tsa.arima_model import ARIMA
ts_log['pm25'] = ts_log['pm25'].astype(float)
date_p = pd.date_range('2018-01-01', '2018-05-01', freq='AS')    
freq = 'H'                                                     
#date = date + pd.Timedelta(3, unit=freq)                       
print(date)
ts_log.index.dtype
ts_log.info()

model = ARIMA(ts_log, order=(3, 0, 2))  
results_AR = model.fit(disp=-1)  
plt.plot(ts_log)
plt.plot(results_AR.fittedvalues, color='red')
plt.title(rmse(ts_log['pm25'], results_AR.fittedvalues))


#MA Model
model = ARIMA(ts_log, order=(3, 0, 2))  
results_MA = model.fit(disp=-1)  
plt.plot(ts_log)
plt.plot(results_MA.fittedvalues, color='red')
plt.title(rmse(ts_log['pm25'], results_AR.fittedvalues))


#Combined Model
model = ARIMA(ts_log, order=(3, 0, 2))  
results_ARIMA = model.fit(disp=-1)  
plt.plot(ts_log)
plt.plot(results_ARIMA.fittedvalues, color='red')
plt.title(rmse(ts_log['pm25'], results_AR.fittedvalues))
#Taking it back to original scale
predictions_ARIMA = pd.Series(results_ARIMA.fittedvalues, copy=True)
print (predictions_ARIMA.head())


# Load specific evaluation tools 
plt.plot(ts_log)
plt.plot(predictions_ARIMA, color='green')
plt.title(rmse(ts_log['pm25'], predictions_ARIMA))

# Fit a SARIMAX(3, 0, 2)x(2, 1, 0, 12) on the training set 

# ============================================================================= 
model = sm.tsa.statespace.SARIMAX(ts_log['pm25'], trend='c', order=(3,0,2))
results = model.fit(disp=False)
print(results.summary())
#
# =============================================================================
#ar = (1)          # this is the maximum degree specification
#ma = (2,1,0,12)  # this is the lag polynomial specification
#model = sm.tsa.statespace.SARIMAX(ts_log['pm25'], trend='c', order=(ar,1,ma))


# =============================================================================
# model = sm.tsa.statespace.SARIMAX(ts_log['pm25'], order=(3,0,2), seasonal_order=(2,1,0,12), simple_differencing=True).fit(dis=-1)
# #results = model.fit(disp=False)
# print(model.summary())
# =============================================================================


results.plot_diagnostics(figsize=(15,12))

predict_date = []
input=300
for i in range(1,input+2):
    predict_date.append(ts_log.index[-1]+pd.DateOffset(hours=i))

forecast = results.predict(start=ts_log.shape[0],end=ts_log.shape[0]+input)    
forecast = pd.DataFrame(forecast)
forecast['date'] = predict_date
forecast = forecast.set_index('date')
forecast = forecast.rename(columns = {'predicted_mean' : 'pm25'})

plt.plot(ts_log)
plt.plot(forecast, color='red')