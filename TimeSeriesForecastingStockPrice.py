
# coding: utf-8

# In[120]:


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
#get_ipython().run_line_magic('matplotlib', 'inline')


# In[122]:


dataset = pd.read_csv("C:\\Users\\aadit\\Downloads\\Python Class\\Python Programs\\TimeSeries\\NSE-TATAGLOBAL.csv")
print(dataset.head())
dataset.info()


# In[182]:


dataset["Date"] = pd.to_datetime(dataset["Date"],infer_datetime_format = True)
dataset.set_index(dataset["Date"], inplace=True)
dataset.info()


# In[156]:


dataset.index


# In[157]:


dataset.info()


# In[158]:


dataset.index.values


# In[159]:


plt.figure(figsize=(40,8))


# In[160]:


plt.xlabel("time")
plt.ylabel("price")
plt.title("chart")
plt.plot(dataset["Date"],dataset["Close"])
#dataset.plot(x = "Date",y = ["Close"])

#plt.plot(dataset["Date"],dataset["Open"], color="green")


# In[161]:


indexedDataset = dataset["Close"]
print(indexedDataset.head())
rollmean = dataset["Close"].rolling(window=12).mean()
rollstd = dataset["Close"].rolling(window=12).std()
#print(rollmean,rollstd)


# In[162]:


plt.figure(figsize=(25,10))
plt.plot(dataset["Date"],dataset["Close"])
plt.plot(rollmean, color="Red", label="Rolling mean")
plt.plot(rollstd, color="Yellow", label="Rolling std")
plt.legend(loc="best")


# In[163]:


#Dickey fuller test for finding if the data is constant
from statsmodels.tsa.stattools import adfuller
dftest = adfuller(dataset["Close"], autolag="AIC")
print(dftest)


# In[164]:


#Log to estimate trend

dataset_logscale = np.log(indexedDataset)
plt.plot(dataset_logscale)


# In[166]:


movingAverage = dataset_logscale.rolling(window=12).mean()
plt.figure(figsize=(25,8))
plt.plot(dataset_logscale)
plt.plot(movingAverage)


# In[168]:


stationary = dataset_logscale - movingAverage
stationary.head(12)


# In[169]:


stationary.dropna(inplace=True)
stationary.head()


# In[170]:


rollmean = stationary.rolling(window=12).mean()
rollstd = stationary.rolling(window=12).std()
#print(rollmean,rollstd)
plt.figure(figsize=(25,10))
plt.plot(stationary, color="Blue",label="Original")
plt.plot(rollmean, color="Red", label="Rolling mean")
plt.plot(rollstd, color="black", label="Rolling std")
plt.legend(loc="best")


# In[171]:


dftest = adfuller(stationary, autolag="AIC")
print(dftest)


# In[173]:


expDecayWeightedAverage = dataset_logscale.ewm(halflife=12, min_periods=0,adjust=True).mean()
expDecayWtStationary = dataset_logscale - expDecayWeightedAverage
rollmean = expDecayWtStationary.rolling(window=12).mean()
rollstd = expDecayWtStationary.rolling(window=12).std()
#print(rollmean,rollstd)
plt.figure(figsize=(25,10))
plt.plot(expDecayWtStationary, color="Blue",label="Original")
plt.plot(rollmean, color="Red", label="Rolling mean")
plt.plot(rollstd, color="black", label="Rolling std")
plt.legend(loc="best")

dftest = adfuller(expDecayWtStationary, autolag="AIC")
print(dftest)


# In[174]:


datasetlogdiffshifting = dataset_logscale - dataset_logscale.shift()
plt.plot(datasetlogdiffshifting)


# In[175]:


datasetlogdiffshifting.dropna(inplace = True)
rollmean = datasetlogdiffshifting.rolling(window=12).mean()
rollstd = datasetlogdiffshifting.rolling(window=12).std()
#print(rollmean,rollstd)
plt.figure(figsize=(25,10))
plt.plot(datasetlogdiffshifting, color="Blue",label="Original")
plt.plot(rollmean, color="Red", label="Rolling mean")
plt.plot(rollstd, color="black", label="Rolling std")
plt.legend(loc="best")

dftest = adfuller(datasetlogdiffshifting, autolag="AIC")
print(dftest)


# In[176]:


from statsmodels.tsa.seasonal import seasonal_decompose

#dataset_logscale = np.log(dataset["Close"])
decomposed = seasonal_decompose(dataset_logscale,model="additive", freq=1)


trend = decomposed.trend
season = decomposed.seasonal
residual = decomposed.resid
plt.figure(figsize=(25,15))
plt.subplot(411)
plt.plot(dataset_logscale, label="Original")
plt.legend(loc="best")
plt.subplot(412)
plt.plot(trend, label="Trend")
plt.legend(loc="best")
plt.subplot(413)
plt.plot(season, label="Season")
plt.legend(loc="best")
plt.subplot(414)
plt.plot(residual, label="Residual")
plt.legend(loc="best")







# In[177]:


from statsmodels.tsa.stattools import acf, pacf

lag_acf = acf(datasetlogdiffshifting,nlags=20)
lag_pacf = pacf(datasetlogdiffshifting,nlags=20)


plt.subplot(121)
plt.plot(lag_acf)
plt.title("ACF")
plt.subplot(122)
plt.plot(lag_pacf)
plt.title("PACF")


# In[180]:


from statsmodels.tsa.arima_model import ARIMA

model = ARIMA(dataset_logscale, order = (1,1,1))
results_AR = model.fit(disp = -1)
plt.figure(figsize=(25,15))
plt.plot(datasetlogdiffshifting)
plt.plot(results_AR.fittedvalues, color="Red")
RSS = sum((results_AR.fittedvalues - datasetlogdiffshifting)**2)
print(RSS)


# In[179]:


model = ARIMA(dataset_logscale, order = (1,1,0))
results_AR = model.fit(disp = -1)
plt.figure(figsize=(25,15))
plt.plot(datasetlogdiffshifting)
plt.plot(results_AR.fittedvalues, color="Red")
RSS = sum((results_AR.fittedvalues - datasetlogdiffshifting)**2)
print(RSS)


# In[144]:


prediction_Arima = pd.Series(results_AR.fittedvalues, copy=True)


# In[145]:


prediction_Arima_cumsum = prediction_Arima.cumsum()


# In[189]:


prediction_Arima_log = pd.Series(dataset_logscale.ix[0],index = dataset_logscale.index)
prediction_Arima_log = prediction_Arima_log.add(prediction_Arima_cumsum, fill_value=0)
prediction_Arima_log.head()


# In[147]:


prediction_AR = np.exp(prediction_Arima_log)


# In[150]:


plt.plot(indexedDataset)
plt.plot(prediction_AR)
dataset_logscale


# In[185]:


plt.figure(figsize=(25,15))
results_AR.predict(1,1354)

