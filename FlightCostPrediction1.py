import pandas as pd
import numpy as np
from datetime import date

# for warnings 
import warnings
warnings.filterwarnings('ignore')

# importing Data set
data_train = pd.read_excel('E:\\P-FlightPrediction\\Flight Prediction-Main\\Data_Train.xlsx')
data_train.head()
data_test = pd.read_excel('E:\\P-FlightPrediction\\Flight Prediction-Main\\Test_set.xlsx')
data_test.head()

# Droping NA Values as they are very less which is known by doing Exploratory analysis
data_train = data_train.dropna(how = 'any').reset_index(drop=True)
data_test = data_test.dropna(how = 'any').reset_index(drop=True)

#to find repeated unique names or values
data_train['Destination'].unique()

# Cleaning Destination
data_train['Destination'] = data_train['Destination'].str.replace('New Delhi','Delhi')
data_test['Destination'] = data_test['Destination'].str.replace('New Delhi','Delhi')

# Cleaning Date_of_Journey

data_train['Date'] =  data_train['Date_of_Journey'].apply(pd.to_datetime).dt.day
data_train['Month'] =  data_train['Date_of_Journey'].apply(pd.to_datetime).dt.month
#data_train['Year'] =  data_train['Date_of_Journey'].apply(pd.to_datetime).dt.year
data_train['Day_of_week'] = data_train['Date_of_Journey'].apply(pd.to_datetime).dt.dayofweek
#del data_train['Date_of_Journey']

data_test['Date'] =  data_test['Date_of_Journey'].apply(pd.to_datetime).dt.day
data_test['Month'] =  data_test['Date_of_Journey'].apply(pd.to_datetime).dt.month
#data_test['Year'] =  data_test['Date_of_Journey'].apply(pd.to_datetime).dt.year
data_test['Day_of_week'] = data_test['Date_of_Journey'].apply(pd.to_datetime).dt.dayofweek
#del data_test['Date_of_Journey']

data_train['Journey_date'] = data_train['Date_of_Journey'].apply(lambda x: date(*map(int, reversed(x.split("/")))))
data_test['Journey_date'] = data_test['Date_of_Journey'].apply(lambda x: date(*map(int, reversed(x.split("/")))))

data_train['Journey_date'].unique()
# for median - print("\nmedian of arr, axis = None : ", np.median(data_train['Date']))
# Assuming Booking day
def booking_day(jour_date):
    ref_date = date(2019,3,1)
    day = jour_date-ref_date
    return [i.days for i in day]

data_train['Booking_day'] = booking_day(data_train['Journey_date'])
data_test['Booking_day'] = booking_day(data_test['Journey_date'])

# Cleaning Dep_Time

data_train['Dep_Time_hour'] =  data_train['Dep_Time'].apply(pd.to_datetime).dt.hour
data_train['Dep_Time_min'] =  data_train['Dep_Time'].apply(pd.to_datetime).dt.minute
#del data_train['Dep_Time']

data_test['Dep_Time_hour'] =  data_test['Dep_Time'].apply(pd.to_datetime).dt.hour
data_test['Dep_Time_min'] =  data_test['Dep_Time'].apply(pd.to_datetime).dt.minute
#del data_test['Dep_Time']

# Cleaning Arrival_Time

data_train['Arrival_Time'] = data_train['Arrival_Time'].astype(str).str[:5]
data_train['Arrival_Time_hour'] =  data_train['Arrival_Time'].apply(pd.to_datetime).dt.hour
data_train['Arrival_Time_min'] =  data_train['Arrival_Time'].apply(pd.to_datetime).dt.minute
#del data_train['Arrival_Time']

data_test['Arrival_Time'] = data_test['Arrival_Time'].astype(str).str[:5]
data_test['Arrival_Time_hour'] =  data_test['Arrival_Time'].apply(pd.to_datetime).dt.hour
data_test['Arrival_Time_min'] =  data_test['Arrival_Time'].apply(pd.to_datetime).dt.minute
#del data_test['Arrival_Time']

# Cleaning Duration (Converting into minutes)
data_train['Durr_min_con'] = (data_train['Duration'].str.replace("h", '*60')
                              .str.replace(' ','+')
                              .str.replace('m','*1').apply(eval))

data_test['Durr_min_con'] = (data_test['Duration'].str.replace("h", '*60')
                             .str.replace(' ','+')
                             .str.replace('m','*1').apply(eval))

data_train['Dur_hr'] = data_train['Duration'].apply(lambda x:int(x.split(' ')[0].strip('h')) if 'h' in x else 0)
data_train['Dur_min'] = (data_train['Duration']
                         .apply(lambda x:(int(x.strip('m')) if 'm' in x else 0) 
                                if len(x.split(' '))==1 else int(x.split(' ')[1].strip('m'))))

data_test['Dur_hr'] = data_test['Duration'].apply(lambda x:int(x.split(' ')[0].strip('h')) if 'h' in x else 0)
data_test['Dur_min'] = (data_test['Duration']
                         .apply(lambda x:(int(x.strip('m')) if 'm' in x else 0) 
                                if len(x.split(' '))==1 else int(x.split(' ')[1].strip('m'))))

# Cleaning Total_Stops

data_train['Total_Stops'] = data_train['Total_Stops'].str.replace('non-stop','0')
data_train['Total_Stops'] = data_train['Total_Stops'].str.replace('stops','')
data_train['Total_Stops'] = data_train['Total_Stops'].str.replace('stop','')
data_train['Total_Stops'] = data_train['Total_Stops'].astype(int)

data_test['Total_Stops'] = data_test['Total_Stops'].str.replace('non-stop','0')
data_test['Total_Stops'] = data_test['Total_Stops'].str.replace('stops','')
data_test['Total_Stops'] = data_test['Total_Stops'].str.replace('stop','')
data_test['Total_Stops'] = data_test['Total_Stops'].astype(int)

# Overnight Journey if arrival time is greater than 5 AM
data_train['Overnight'] = np.where([x>5 for x in data_train['Arrival_Time_hour']],1,0)
data_test['Overnight'] = np.where([x>5 for x in data_test['Arrival_Time_hour']],1,0)

# Journey More than a day
data_train['Fullday_Flag'] = np.where([x>1450 for x in data_train['Durr_min_con']],1,0)
data_test['Fullday_Flag'] = np.where([x>1450 for x in data_test['Durr_min_con']],1,0)

# Cleaning Additional Info
data_train['Additional_Info'] = data_train['Additional_Info'].replace('No Info','No info')
data_test['Additional_Info'] = data_test['Additional_Info'].replace('No Info','No info')

# Creating a Flag if it is a weekend
data_train['Weekend_Flag'] = data_train['Day_of_week'].apply(lambda x:1 if x in [4,6] else 0)
data_test['Weekend_Flag'] = data_test['Day_of_week'].apply(lambda x:1 if x in [4,6] else 0)

# Creating a Flag for Bussiness class Airline
data_train['luxury_flag'] = data_train['Airline'].apply(lambda x:1 if x in ['Jet Airways Business'] else 0)
data_test['luxury_flag'] = data_test['Airline'].apply(lambda x:1 if x in ['Jet Airways Business'] else 0)

# Function for Route Column
def stop(route,n):
    stops = route.split(' → ')
    if (len(stops)>=n+2):
        return route.split(' → ')[n]
    else:
        return '0'

#    Spliting Route column
for i in range(1,max(data_train['Total_Stops'])+1):
    data_train['Stop_'+str(i)] = data_train['Route'].apply(lambda x: stop(x,i))
    

for i in range(1,max(data_test['Total_Stops'])+1):
    data_test['Stop_'+str(i)] = data_test['Route'].apply(lambda x: stop(x,i))

data_train =  (data_train[['Airline','Date_of_Journey','Booking_day','Date','Month','Day_of_week',
                           'Dep_Time_hour','Dep_Time_min','Arrival_Time_hour','Arrival_Time_min','Source','Destination',
                           'Stop_1','Stop_2','Stop_3','Stop_4','Durr_min_con','Dur_hr','Dur_min','Total_Stops',
                           'Additional_Info','Overnight','Weekend_Flag','luxury_flag','Fullday_Flag','Price']])

data_test =  (data_test[['Airline','Date_of_Journey','Booking_day','Date','Month','Day_of_week',
                           'Dep_Time_hour','Dep_Time_min','Arrival_Time_hour','Arrival_Time_min','Source','Destination',
                           'Stop_1','Stop_2','Stop_3','Stop_4','Durr_min_con','Dur_hr','Dur_min','Total_Stops',
                           'Additional_Info','Overnight','Weekend_Flag','luxury_flag','Fullday_Flag']])

# Assigning X and y

X = data_train.iloc[:,data_train.columns != 'Price']
y = data_train.Price

X['label'] = 'train'
data_test['label'] = 'test'

# Concat
concat_data = pd.concat([X , data_test])

# Creating Dummy_list of column for Encoding
categorical = []
for col, value in data_train.iteritems():
    if value.dtype == 'object':
        categorical.append(col)

# Function for Dummies
def dummy_df(df, categorical):
    for x in categorical:
        dummies = pd.get_dummies(df[x], prefix=x, dummy_na=False)
        df = df.drop(x,1)
        df = pd.concat([df, dummies], axis=1)
    return df

concat_data = dummy_df(concat_data,categorical)
concat_data.head()

X = concat_data[concat_data['label'] == 'train']
data_test = concat_data[concat_data['label'] == 'test']

X = X.drop('label', axis=1)
data_test = data_test.drop('label', axis=1)

# Scaling
from sklearn.preprocessing import StandardScaler
sc = StandardScaler()

# Scaling Train data dependent
X = sc.fit_transform(X)
# Scaling Test data dependent
data_test = sc.transform(data_test)

# Scaling Train data Independent
y = y.values
y = y.reshape((len(y), 1)) 
y = sc.fit_transform(y)
y = y.ravel()

# Importing Random Forest Regressor model

from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X,y,test_size=0.22, random_state =13)

from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import r2_score
from sklearn.model_selection import GridSearchCV
model = RandomForestRegressor(n_estimators=2000,random_state=10,max_depth=50,max_features=0.5)
# n_estimators=500,random_state=10,max_depth=20,max_features=0.5

#param_grid = {'n_estimators': [500, 2000],'max_depth' : [1,50],'random_state' :[10,50]}

#model_GRS = GridSearchCV(estimator=model, param_grid=param_grid, cv= 5)
#model_GRS.fit(X_train,y_train)

model.fit(X_train,y_train)
y_predict = model.predict(X_test)
r2_score(y_predict,y_test)

#print (model_GRS.best_params_)
# {'max_depth': 50, 'n_estimators': 2000, 'random_state': 10}

# Importing Cross Validation

from sklearn.model_selection import cross_val_score, cross_val_predict

# Prediction Score
y_predict_CV = cross_val_predict(model, X,y, cv=5)
r2_score(y,y_predict_CV)

from math import sqrt
from sklearn.metrics import mean_squared_error, mean_squared_log_error

#error = sqrt(mean_squared_log_error(y_test, y_predict))
#print("RMSLE value is =",error)
#print(1-error)

y_predict_test =model.predict(data_test)
y_predict_test = sc.inverse_transform(y_predict_test)
y_predict_test = pd.DataFrame(y_predict_test)
y_predict_test = y_predict_test.astype(int)
y_predict_test.to_excel('E:\\P-FlightPrediction\\Flight Prediction-Main\\Train_Test data.xlsx')
