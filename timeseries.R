#time series r code
install.packages("forecast")
library(forecast)
library(tidyverse)

#read in csv file
Waittime<-read.csv("WaittimeOBPractices.csv")

#assign time start period and frequency of time period in data
#here we are looking at 12 months
WaitTimeSeries<-ts(Waittime$mean, start=c(2017,1), freq=12)

#view how r sorted dates
time(WaitTimeSeries)

#plot time series data
WaittimeGraph<-plot(WaitTimeSeries)

#add trend line
WaittimeGraph<-abline(reg = lm(WaitTimeSeries~time(WaitTimeSeries)))

plot(decompose(WaitTimeSeries))

#auto correlation function 
acf(WaitTimeSeries)

plot(HoltWinters(WaitTimeSeries, alpha = 0.001, beta=1,gamma=0))

#forecasting wait time mean for the next ten years
HW.Wait<-HoltWinters(WaitTimeSeries)
plot(HW.Wait)
HWpredict<-predict(HW.Wait, n.ahead =10*12)
class(HWpredict)
HWpredict
ts.plot(WaitTimeSeries,HWpredict, lty=1:2)

