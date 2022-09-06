##Foreign Exchange Rates
# Cleaning the exchange rates data frame
#Aim is to clear the null values for easier manipulation
View(Foreign_Exchange_Rates)
foreign_rates = na.omit(Foreign_Exchange_Rates)
View(foreign_rates)
summary(foreign_rates)

###Analysis of the Brent Crude Oil Prices
# Line plot of the Brent Crude oil prices against the returns from its sales
#This is essential to see how the index has been performing over the years
library(ggplot2)
library(dplyr)
brent_line<-ggplot(data=brent_crudeoil_data, aes(x=Date, y=Close, group=1)) +
  geom_line(color="red",size=0.5)+
  geom_point()
brent_line+ggtitle("Brent Crude Oil Index Performance")+
  labs(x="Years",y="Price Levels")
#There is a clear significance price drop around the post 2007 before 2008 era where the world suffered a great economic downturn
#Indicative of the effect of worldwide recessions on their impact on oil prices
#Around the 2020 Covid period of recession there is also a decline in the price levels.

## OLS analysis on the relationship between the foreign exchange rates and the prices of the Brent Crude Oil
brent_prices=brent_crudeoil_data[1:5013,5]
View(brent_prices)
#H0:Exchange rate has no influence on the oil prices
#Ha:Exchange rate has an influence on the oil prices
foreign_model<- lm(brent_prices$Close ~ foreign_rates$`EURO AREA - EURO/US$`)
summary(foreign_model)
#With the p-value of < 2.2e-16 , viewed against our significance level of 0.05 leads us to reject H0 indicating that the exchange rates do influence the prices.
foreign_model$coefficients
#The Beta coefficient of -171.0840 indicates that as the exchange rate increases by one unit, the oil prices decrease by the beta value.

# Upon assessing the correlation between the two variables , we see a strong negative correlation -0.7161255.
#Indicating that the variables move in opposite directions.
cor(foreign_rates$`EURO AREA - EURO/US$`,brent_prices$Close)

foreign_model%>%
  ggplot(aes(x = foreign_rates$`EURO AREA - EURO/US$`, y = brent_prices$Close)) +
  geom_point(colour = "blue") +
  geom_smooth(method = "lm", fill = NA)
# This plot shows us that as the exchange rate is high, the oil prices decrease. By supply and demand analysis
# the lower exchange rate causes the demand for oil to increase as the countries can now purchase oil barrels cheaply, this causes the suppliers to increase the price levels.
#On the other hand, as the exchange rate increases, the price levels decrease because it is costlier to purchase oil barrels leading to a demand decrease.
#this causes those on the supply side to lower their prices to meet the demand levels.

# Cleaning the OECD Crude Oil Production Data
# Have a constant time column and volume column and location column
OECD_data2 = subset(OECD_Data_Crude_Oil_Production,select= -c(2,3,4,5,8))
View(OECD_data2)
OECD_data= na.omit(OECD_data2)

View(OECD_data)
summary(OECD_data)

# Running an OLS regression to analyze the nature of the relationship between the production levels of OECD countries and the prices of oil
#H0: production levels have no effect on the oil prices
#Ha: production levels have an effect on the oil prices
production2=log(OECD_data[1:5013,3])
View(production2)
production=(OECD_data[1:5013,3])
OECD_model<- lm(brent_prices$Close ~ production$Value)
summary(OECD_model)
#With the p-value of 5.724e-05 ,viewed against our significance level of 0.05 leads us to reject H0 indicating that the production values do influence the prices.
OECD_model$coefficients
#The Beta coefficient of 4.801662e-06 indicates that as the production values increases by one unit, the oil prices increase by the beta value.

# Upon assessing the correlation between the two variables , we see a positive correlation of 0.05680093. 
# This indicates that the variables overtime move in the same direction.
cor(production$Value,brent_prices$Close)

#The plot shows further the lower prices where the production values as greater. Applying supply and demand analysis
#when the supply levels are higher, assuming constant demand, the prices go below the equilibrium levels.
OECD_model%>%
  ggplot(aes(x = production2$Value, y =brent_prices$Close)) +
  geom_point(colour = "green") +
  geom_smooth(method = "lm", fill = NA)

## TESTING FOR STATIONARITY
# Testing for stationarity in the Brent Crude Oil price Data

library(tseries)
#H0: the data is non-stationary
#Ha: the data is stationary
adf.test(brent_crudeoil_data$Close)
# The large p-value of 0.5689 shows that the probability of observing the null is high hence the data is non-stationary.
par(mfrow = c(1, 2))
acf(brent_crudeoil_data$Close, main = "Correlogram of Brent Oil Prices-before differencing", lag.max = 40)
#The correlogram further indicates that the data is non-stationary with the significantly high autocorrelation coefficients that decay to zero slowly

# To correct for this, we difference the data
brent_stat<-diff(brent_crudeoil_data$Close, differences = 1) 
plot.ts(brent_stat) 
# When plotted it shows the oscillation around the mean that is typical of a white noise process.
adf.test(brent_stat)
# Upon running the Dickey-Fueller test, we get a p-value of 0.01 which is smaller than our 0.05 level of significance
# Hence we reject the H0 , showing that the data is indeed stationary.
par(mfrow = c(1, 2))
acf(brent_stat, main = "Correlogram of Brent Oil Prices-after differencing", lag.max = 40)

# Testing for stationarity in the Foreign Exchange Rates data
adf.test(foreign_rates$`EURO AREA - EURO/US$`)
# The large p-value of 0.8005 shows that the probability of observing the null is high hence the data is non-stationary.
par(mfrow = c(1, 2))
acf(foreign_rates$`EURO AREA - EURO/US$`, main = "Correlogram of Euro/US exchange rates-before differencing", lag.max = 40)
#The correlogram further indicates that the data is non-stationary with the significantly high autocorrelation coefficients that decay to zero slowly
# To correct for this, we difference the data
foreign_stat<-diff(foreign_rates$`EURO AREA - EURO/US$`, differences = 1) 
plot.ts(foreign_stat) 
# When plotted it shows the oscillation around the mean that is typical of a white noise process.
adf.test(foreign_stat)
# Upon running the Dickey-Fueller test, we get a p-value of 0.01 which is smaller than our 0.05 level of significance
# Hence we reject the H0 , showing that the data is indeed stationary.
par(mfrow = c(1, 2))
acf(foreign_stat, main = "Correlogram of Euro/US exchange rates-after differencing", lag.max = 40)

# Testing for stationarity in the OECD Oil Production data
adf.test(production$Value)
# Upon running the Dickey-Fueller test, we get a p-value of 0.01 which is smaller than our 0.05 level of significance
# Hence we reject the H0 , showing that the data is indeed stationary.
par(mfrow = c(1, 2))
acf(foreign_stat, main = "Correlogram of OECD Oil Production Data", lag.max = 40)

##UNIVARIATE TIME SERIES ANALYSIS
#Foreign Exchange rates
acf(foreign_stat, main = "Correlogram of Euro/US exchange rates", lag.max = 40)
pacf(foreign_stat, main = "Partial Correlogram of Euro/US exchange rates", lag.max = 40)
# The Partial correlogram is showing significant spikes which are characteristic of an AR(1)model.

##ESTIMATION
library(forecast)
foreign_model<-auto.arima(foreign_stat)
summary(foreign_model)
# The results of the ARMA test indicate that we have a model of AR(1)


## DIAGNOSTIC CHECKING
#The H0: no joint significance in the autocorrelation of error terms
#The Ha: joint significance in the autocorrelation of error terms
Box.test(residuals(foreign_model), lag = 60, type = c("Ljung-Box"))
# The p-value of 0.3689 compared against our significance level of 0.05 leads us to fail to reject Ho
# Indicating that there is no joint significance in the autocorrelation of error terms.

## TESTING FOR COINTEGRATION
# Johansen Cointegration Test
production$Value<-ts(production$Value)
brent_prices$Close<-ts(brent_prices$Close)
foreign_rates$`EURO AREA - EURO/US$`<-ts(foreign_rates$`EURO AREA - EURO/US$`)
database <- window(ts.union(production$Value,brent_prices$Close,foreign_rates$`EURO AREA - EURO/US$`))
library(urca)

#Running a VARselect to get the optimal number of lags to use in the model
library(vars)
VARselect(y=database, lag.max = 9, type = "const")       
# With two lags, the AIC is at its lowest value of 12.57701 and the BIC at a low value of 12.60436.
# It follows that the optimal number of lags is 2.
jotest=ca.jo(database, type="trace", K=2, ecdet="const")
summary(jotest)
# We reject zero cointegrating relationships because the test statistic of 96.30 is greater than the 5% critical value of 34.91.
# Similarly, we reject one cointegrating relationship because the test statistic of 25.55 is greater than the 5% critical value of 19.96.
# We fail to reject two cointegrating relationships because the test statistic of 3.72 is less than the critical value at the 5% significance level of 9.24.

##MULTIVARIATE TIME SERIES ANALYSIS
#Modelling using VECM Models
library(tseries)
# Testing and correcting for Stationarity
adf.test(production$Value)
adf.test(diff(brent_prices$Close),k=1)
adf.test(diff(foreign_rates$`EURO AREA - EURO/US$`),k=1)
database2 <- window(ts.union(production$Value,brent_prices$Close,foreign_rates$`EURO AREA - EURO/US$`))
# Choosing the optimal number of lags
VARselect(y=database2, lag.max = 8, type = "none")  
#The optimal number of lags is two because of the minimum value of the AIC at 12.57926 and the low BIC value at 12.60270.
#Running the Johansen Cointegration Test
jotest2=ca.jo(database2, type="trace", K=2, ecdet="const")
summary(jotest2)

###USING CAJORLS TO ESTIMATE THE VECM MODEL
brent_oil <- cajorls(jotest, r = 2)
summary(brent_oil$rlm)
# There are two error correction terms for the two cointegrating relationships 2.536e-02 and 7.376e+01. 
# Similarly, there are several short term coefficients between the other variables
#The long run equilibrium equations
brent_oil$beta
# There is a long-run relationship between the value of production and the foreign exchange rates
#This relationship has a beta of 79590.48.
# There is also a long-run relationship between the prices of the Brent Crude oil and the foreign exchange rates.
# This relationship has a beta of 2.187971e+02.
