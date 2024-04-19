suppressMessages(library(tidyverse))
suppressMessages(library(ggplot2))
suppressMessages(library(zoo))
suppressMessages(library(aTSA))
suppressMessages(library(tseries))
suppressMessages(library(forecast))
suppressMessages(library(lubridate))


dat <- read.csv("/Users/hritikchhabra/Downloads/owid-covid-data.csv")
class(dat$date)

#converting date column in date format
dat$date <- as.Date(dat$date)
class(dat$date)

dat_In <- dat[dat$location == "India",c(3,4,7,8)]

p <- ggplot(dat_In, aes(x=date, y=new_deaths)) +
  geom_line() + scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  xlab("Months") + ylab("New Deaths")+ggtitle("Number of deaths in the India")+geom_area(fill="lightblue", color="black")
p

# Let plot the mean to check the first condition if our data is stationary or not.
p + geom_smooth(method = lm, col = "red", se = FALSE)

# Change the data to zoo type
dat_In_Analysis <- dat_In[,c(2,4)]
dat_demo <- zoo(dat_In_Analysis$new_deaths, seq(from = as.Date("2019-12-31"), to = as.Date("2020-08-01"), by = 1))

class(dat_demo)

summary(dat_demo)

plot((dat_demo))


# Make the data stationary, by differencing the data.
stationary_data <- diff(dat_demo)

plot(stationary_data)

# To check if it's stationary we conduct a quantitative test. We use the Augmented Dickey-Fuller Test.
# H_0 = The null hypothesis for this test is that there is a unit root.
# H_A = The alternative hypothesis is that the time series is stationary (or trend-stationary).
adf.test(as.matrix(stationary_data)) 

# We use the Auto Correlation Graph

# First we have a look at our acf graph when our data isn't stationary.
acf(dat_demo)
acf(stationary_data)


pacf(stationary_data)
plot(stationary_data)

stationary_data


# arima has a auto.arima function which gives us the ideal arima model based on our data.
arima_funct <- auto.arima(dat_demo)
res<- residuals(arima_funct)
LBtest <- Box.test(res, type="Ljung-Box", lag=20)

arima_funct

# lets use the auto.arima function to forecast 3 weeks

forecast1 <- forecast(arima_funct, h=21)

round(sum(forecast1$upper[,2]),0)



additional_deaths <- round(sum(forecast1$upper[,2]),0)



# [1] 20419(original is 19000) this is additional

total_number_of_deaths <- round(sum(dat_In_Analysis$new_deaths)+additional_deaths,0)
# [1] 56930(original is 55794) this is cumulative

# lets use the auto.arima function to forecast 3 months
forecast2 <- forecast(object = arima_funct, h = 90)
additional_deaths2 <- round(sum(forecast2$upper[,2]),0)
# [1] 103347 death in 90 days(original is 87000) this is additional

total_number_of_deaths2 <- round(sum(dat_In_Analysis$new_deaths)+additional_deaths2,0)
# [1] 139958(original is 123000) this is cumulative
plot(forecast1)


delta <- (forecast1$lower[,2]+forecast1$upper[,2])/2

predicted_21 <- data.frame(date=seq(from=18476,by=1,length=21),new_deaths=delta)
original <- data.frame(date=as.numeric(dat_In_Analysis$date),new_deaths=c(0,stationary_data[-1]))
z <- rbind(original,predicted_21)
z$date <- as.Date(z$date)


forecast1_data <- as.data.frame(forecast1)
forecast1_data <- data.frame(date=seq(from=18476,by=1,length=21),forecast1_data, y = delta)
forecast1_data$date <- as.Date(forecast1_data$date)

graph <- ggplot(data=z,aes(x=date,y=new_deaths),show.legend = FALSE)+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  geom_line(data=original,aes(x=as.Date(date),y=new_deaths))+
  geom_ribbon(data=forecast1_data,aes(x = date,ymin =Lo.95, ymax =Hi.95), inherit.aes = FALSE,fill = "lightsteelblue2")+
  geom_ribbon(data=forecast1_data,aes(x = date,ymin =Lo.80, ymax =Hi.80), inherit.aes = FALSE,fill = "lightsteelblue3")+
  geom_line(data=forecast1_data,aes(x=date,y=y),size=1,color='purple')+
  ggtitle("Forecasts from ARIMA(0,1,1) with zero mean")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")+
  labs(x="Date",y="New Deaths")


delta2 <- (forecast2$lower[,2]+forecast2$upper[,2])/2

predicted_22 <- data.frame(date=seq(from=18476,by=1,length=nrow(forecast2$lower)),new_deaths=delta2) # prediction from ARIMA
original <- data.frame(date=as.numeric(dat_In_Analysis$date),new_deaths=c(0,stationary_data[-1])) # Previous Observations 
z2 <- rbind(original,predicted_22) # combine the two data sets
z2$date <- as.Date(z2$date) # convert numeric column to date (creating the base layer of ggplot)

forecast2_data <- as.data.frame(forecast2) # convert forecast data into data.frame
forecast2_data <- data.frame(date=seq(from=18476,by=1,length=90),forecast2_data, y = delta2) #add a date and delta of Lo.95 & Hi.95
forecast2_data$date <- as.Date(forecast2_data$date) # covert to date 

graph <- ggplot(data=z2,aes(x=date,y=new_deaths),show.legend = FALSE)+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  geom_line(data=original,aes(x=as.Date(date),y=new_deaths))+
  geom_ribbon(data=forecast2_data,aes(x = date,ymin =Lo.95, ymax =Hi.95), inherit.aes = FALSE,fill = "lightsteelblue2")+
  geom_ribbon(data=forecast2_data,aes(x = date,ymin =Lo.80, ymax =Hi.80), inherit.aes = FALSE,fill = "lightsteelblue3")+
  geom_line(data=forecast2_data,aes(x=date,y=y),size=1,color='purple')+
  ggtitle("Forecasts from ARIMA(0,1,1) with zero mean")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")+
  labs(x="Date",y="New Deaths")


