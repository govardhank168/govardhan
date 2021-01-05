import numpy as np
import pandas as pd
Amtrak = pd.read_csv("E:\\Excelr Data\\RCodes\\Forecasting\\trakdata.csv")
#month =['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'] 
#import numpy as np
#p = Amtrak["Month"][0]
#p[0:3]
#Amtrak['months']= 0

#for i in range(159):
#    p = Amtrak["Month"][i]
#    Amtrak['months'][i]= p[0:3]
    
#month_dummies = pd.DataFrame(pd.get_dummies(Amtrak['months']))
#Amtrak1 = pd.concat([Amtrak,month_dummies],axis = 1)

#Amtrak1["t"] = np.arange(1,160)

#Amtrak1["t_squared"] = Amtrak1["t"]*Amtrak1["t"]
#Amtrak1.columns
#Amtrak1["log_Rider"] = np.log(Amtrak1["Ridership "])
#Amtrak1.rename(columns={"Ridership ": 'Ridership'}, inplace=True)
#Amtrak1.Ridership.plot()
Train = Amtrak.head(147)
Test = Amtrak.tail(12)

# to change the index value in pandas data frame 
# Test.set_index(np.arange(1,13))

####################### L I N E A R ##########################
import statsmodels.formula.api as smf 

linear_model = smf.ols('Ridership~t',data=Train).fit()
pred_linear =  pd.Series(linear_model.predict(pd.DataFrame(Test['t'])))
rmse_linear = np.sqrt(np.mean((np.array(Test['Ridership'])-np.array(pred_linear))**2))
rmse_linear

##################### Exponential ##############################

Exp = smf.ols('log_rider~t',data=Train).fit()
pred_Exp = pd.Series(Exp.predict(pd.DataFrame(Test['t'])))
rmse_Exp = np.sqrt(np.mean((np.array(Test['Ridership'])-np.array(np.exp(pred_Exp)))**2))
rmse_Exp

#################### Quadratic ###############################

Quad = smf.ols('Ridership~t+t_square',data=Train).fit()
pred_Quad = pd.Series(Quad.predict(Test[["t","t_square"]]))
rmse_Quad = np.sqrt(np.mean((np.array(Test['Ridership'])-np.array(pred_Quad))**2))
rmse_Quad

################### Additive seasonality ########################

add_sea = smf.ols('Ridership~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data=Train).fit()
pred_add_sea = pd.Series(add_sea.predict(Test[['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov']]))
rmse_add_sea = np.sqrt(np.mean((np.array(Test['Ridership'])-np.array(pred_add_sea))**2))
rmse_add_sea

################## Additive Seasonality Quadratic ############################

add_sea_Quad = smf.ols('Ridership~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data=Train).fit()
pred_add_sea_quad = pd.Series(add_sea_Quad.predict(Test[['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','t','t_square']]))
rmse_add_sea_quad = np.sqrt(np.mean((np.array(Test['Ridership'])-np.array(pred_add_sea_quad))**2))
rmse_add_sea_quad 

################## Multiplicative Seasonality ##################

Mul_sea = smf.ols('log_rider~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data = Train).fit()
pred_Mult_sea = pd.Series(Mul_sea.predict(Test))
rmse_Mult_sea = np.sqrt(np.mean((np.array(Test['Ridership'])-np.array(np.exp(pred_Mult_sea)))**2))
rmse_Mult_sea

##################Multiplicative Additive Seasonality ###########

Mul_Add_sea = smf.ols('log_rider~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data = Train).fit()
pred_Mult_add_sea = pd.Series(Mul_Add_sea.predict(Test))
rmse_Mult_add_sea = np.sqrt(np.mean((np.array(Test['Ridership'])-np.array(np.exp(pred_Mult_add_sea)))**2))
rmse_Mult_add_sea 

################## Testing #######################################

data = {"MODEL":pd.Series(["rmse_linear","rmse_Exp","rmse_Quad","rmse_add_sea","rmse_add_sea_quad","rmse_Mult_sea","rmse_Mult_add_sea"]),"RMSE_Values":pd.Series([rmse_linear,rmse_Exp,rmse_Quad,rmse_add_sea,rmse_add_sea_quad,rmse_Mult_sea,rmse_Mult_add_sea])}
table_rmse=pd.DataFrame(data)
table_rmse
# so rmse_add_sea has the least value among the models prepared so far 
# Predicting new values 

predict_data = pd.read_csv("E:\\Excelr Data\\Python Classes\\Forecasting_Python\\Predict_new.csv")
model_full = smf.ols('Ridership~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov',data=Amtrak).fit()

pred_new  = pd.Series(add_sea_Quad.predict(predict_data))
pred_new

predict_data["forecasted_Ridership"] = pd.Series(pred_new)
