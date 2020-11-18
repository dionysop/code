library(xts)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(urca) 
library(rstatix)
library(fGarch)
library(forecast)
library(tseries)
###########################
#    Insert the data      #
###########################

#First the Data-Dataset
data_dates1_ts<- read.table("Data_dates1.txt",  header = T)
data_values1_ts<- read.table("Data_values1.txt",  header = T)
DAX <- cbind(data_dates1_ts, data_values1_ts) 
rm(data_dates1_ts, data_values1_ts)
DAX$DAX <- as.numeric(gsub(",", ".", DAX$DAX))


data_dates2_ts<- read.table("Data_dates2.txt",  header = T)
data_values2_ts<- read.table("Data_values2.txt",  header = T)
XA <- cbind(data_dates2_ts, data_values2_ts) 
rm(data_dates2_ts, data_values2_ts)
XA$FTSE.XA_LARGE_CAP <- as.numeric(gsub(",", ".", XA$FTSE.XA_LARGE_CAP))

data_dates3_ts<- read.table("Data_dates3.txt",  header = T)
data_values3_ts<- read.table("Data_values3.txt",  header = T)
LSE <- cbind(data_dates3_ts, data_values3_ts) 
rm(data_dates3_ts, data_values3_ts)
LSE$FTSE_100.LSE <- as.numeric(gsub(",", ".", LSE$FTSE_100.LSE))

#########################################################
#Transform the data into appropriate an appealing form  #
#########################################################

DAX$Dates <- as.Date(DAX$Dates, format ="%d/%m/%Y")
XA$Dates <- as.Date(XA$Dates,"%d.%m.%Y")
LSE$Dates <- as.Date(LSE$Dates,"%d/%m/%Y")

DAX_ts <- xts(DAX$DAX , order.by=DAX$Dates)
names(DAX_ts)[1] <- "DAX"
DAX_ts$DAX <- DAX$DAX

XA_ts <- xts(XA$FTSE.XA_LARGE_CAP , order.by=XA$Dates)
names(XA_ts)[1] <- "FTSE.XA_LARGE_CAP"
XA_ts$FTSE.XA_LARGE_CAP <- XA$FTSE.XA_LARGE_CAP

LSE_ts <- xts(LSE$FTSE_100.LSE , order.by=LSE$Dates)
names(LSE_ts)[1] <- "FTSE_100.LSE"
LSE_ts$FTSE_100.LSE <- LSE$FTSE_100.LSE

summary(DAX_ts)
summary(XA_ts)
summary(LSE_ts)

par(mfrow=c(1,3))
plot(DAX_ts)
plot(XA_ts,col="blue")
plot(LSE_ts,col="red")
par(mfrow=c(1,1))

######################################################################
#Generate Data frame as a combination of DAX, XA, LSE time  series   #
######################################################################

Comb_ts <- cbind(XA_ts,DAX_ts,LSE_ts)
Comb_ts <- Comb_ts[complete.cases(Comb_ts), ]


ggplot(Comb_ts, aes(x = index(Comb_ts))) +
  geom_line(aes(y = Comb_ts$DAX, color = "DAX")) + ggtitle("XA-DAX-LSE series") +
  geom_hline(yintercept=mean(Comb_ts$DAX),color = "gray40", size=1)+
  geom_line(aes(y = Comb_ts$FTSE.XA_LARGE_CAP , color = "XA")) +
  geom_hline(yintercept=mean(Comb_ts$FTSE.XA_LARGE_CAP),color = "firebrick4", size=1)+
  geom_line(aes(y = Comb_ts$FTSE_100.LSE, color = "LSE")) + xlab("Date") + ylab("Price") +
  geom_hline(yintercept=mean(Comb_ts$FTSE_100.LSE),color = "darkcyan", size=1)+
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  scale_colour_manual("Series", values=c("DAX"="gray40", "XA"="firebrick4", "LSE"="darkcyan"))  +
  scale_x_date(date_labels = "%b %y", date_breaks = "6 months") 

##############################################
#Try to make The time-series stationaries    #
##############################################
DAX_Ret_ts <- DAX_ts[-1,]
n <- nrow(DAX_ts)
DAX_Ret_ts$DAX <- log(DAX$DAX[2:n]/DAX$DAX[1:(n-1)])

XA_Ret_ts <- XA_ts[-1,]
n <- nrow(XA_ts)
XA_Ret_ts$FTSE.XA_LARGE_CAP <- log(XA$FTSE.XA_LARGE_CAP[2:n])-log(XA$FTSE.XA_LARGE_CAP[1:(n-1)])

LSE_Ret_ts <- LSE_ts[-1,]
n <- nrow(LSE_ts)
LSE_Ret_ts$FTSE_100.LSE <- log(LSE$FTSE_100.LSE[2:n]/LSE$FTSE_100.LSE[1:(n-1)])


par(mfrow=c(3,2)) # plot the new time series
plot(DAX_ts$DAX)
plot(DAX_Ret_ts$DAX)

plot(XA_ts$FTSE.XA_LARGE_CAP,col="blue")
plot(XA_Ret_ts$FTSE.XA_LARGE_CAP,col="blue")

plot(LSE_ts$FTSE_100.LSE,col="red")
plot(LSE_Ret_ts$FTSE_100.LSE,col="red")

par(mfrow=c(3,2))  # set up the graphics  
plot(DAX_Ret_ts$DAX,type="l", lwd=1,main="Differences of log of DAX")
hist(DAX_Ret_ts$DAX, nclass=15, main="Histogram of differences of log of DAX")

plot(XA_Ret_ts$FTSE.XA_LARGE_CAP,col="blue",type="l", lwd=1,main="Differences of log of XA")
hist(XA_Ret_ts$FTSE.XA_LARGE_CAP, nclass=15, main="Histogram of differences of log of XA")

plot(LSE_Ret_ts$FTSE_100.LSE,col="red",type="l", lwd=1,main="Differences of log of LSE")
hist(LSE_Ret_ts$FTSE_100.LSE, nclass=15 ,main="Histogram of differences of log of LSE")


##################
# normality test #
##################

shapiro.test(as.vector(DAX_Ret_ts$DAX))
shapiro.test(as.vector(XA_Ret_ts$FTSE.XA_LARGE_CAP))
shapiro.test(as.vector(LSE_Ret_ts$FTSE_100.LSE))

par(mfrow=c(3,2))         

hist(DAX_Ret_ts$DAX, prob=TRUE, 15)    # histogram    
lines(density(DAX_Ret_ts$DAX))             # smooth it - ?density for details 
qqnorm(DAX_Ret_ts$DAX,main="Normal QQplot of DAX")      # normal Q-Q plot  
qqline(DAX_Ret_ts$DAX)    

hist(XA_Ret_ts$FTSE.XA_LARGE_CAP, prob=TRUE, 15)    # histogram    
lines(density(XA_Ret_ts$FTSE.XA_LARGE_CAP),col="blue")             # smooth it - ?density for details 
qqnorm(XA_Ret_ts$FTSE.XA_LARGE_CAP,main="Normal QQplot of DAX")      # normal Q-Q plot  
qqline(XA_Ret_ts$FTSE.XA_LARGE_CAP,col="blue")  

hist(DAX_Ret_ts$DAX, prob=TRUE, 15)    # histogram    
lines(density(DAX_Ret_ts$DAX),col="red")             # smooth it - ?density for details 
qqnorm(LSE_Ret_ts$FTSE_100.LSE,main="Normal QQplot of DAX")      # normal Q-Q plot  
qqline(LSE_Ret_ts$FTSE_100.LSE,col="red")  

##################
# Unit Root test #
##################
#Ljung-Box tests based on 12 lags on the return series
Box.test(DAX_Ret_ts,lag=12,type="Ljung")
Box.test(DAX_Ret_ts^2,lag=12,type="Ljung")
DAX_unit_root = ur.df(DAX_Ret_ts,type="trend" , lags=6)
summary (DAX_unit_root)

par(mfrow=c(2,2))  # set up the graphics  
acf(DAX_Ret_ts$DAX, 48, main="ACF of differences of  log of DAX")       
pacf(DAX_Ret_ts$DAX, 48, main="PACF of differences of  log of DAX")
acf(DAX_Ret_ts$DAX^2, 48, main="ACF of differences of  log of DAX")       
pacf(DAX_Ret_ts$DAX^2, 48, main="PACF of differences of  log of DAX")

Box.test(XA_Ret_ts,lag=12,type="Ljung")
Box.test(XA_Ret_ts^2,lag=12,type="Ljung")
XA_unit_root = ur.df(XA_Ret_ts,type="trend" , lags=6)
summary (XA_unit_root)

par(mfrow=c(2,2))  # set up the graphics  
acf(XA_Ret_ts$FTSE.XA_LARGE_CAP, 48, main="ACF of differences of  log of XA")       
pacf(XA_Ret_ts$FTSE.XA_LARGE_CAP, 48, main="PACF of differences of  log of XA")
acf(XA_Ret_ts$FTSE.XA_LARGE_CAP^2, 48, main="ACF of differences of  log of XA")       
pacf(XA_Ret_ts$FTSE.XA_LARGE_CAP^2, 48, main="PACF of differences of  log of XA")

Box.test(LSE_Ret_ts,lag=12,type="Ljung")
Box.test(LSE_Ret_ts^2,lag=12,type="Ljung")
LSE_unit_root = ur.df(LSE_Ret_ts,type="trend" , lags=6)
summary (LSE_unit_root)

par(mfrow=c(2,2))  # set up the graphics  
acf(LSE_Ret_ts$FTSE_100.LSE, 48, main="ACF of differences of  log of LSE")       
pacf(LSE_Ret_ts$FTSE_100.LSE, 48, main="PACF of differences of  log of LSE")
acf(LSE_Ret_ts$FTSE_100.LSE^2, 48, main="ACF of differences of  log of LSE^2")       
pacf(LSE_Ret_ts$FTSE_100.LSE^2, 48, main="PACF of differences of  log of LSE^2")


#############################
#Problems:                  #
#1)Autocorelated time-Series#
# Volatility issues:        #
#1)Fat-tails                #
#2)Volatility clustering    #
#3)Heterosketasity          #
#############################

#########################
# Autocorelation issues #
#########################
DAX.arima <- auto.arima(DAX_Ret_ts)

#DIAGNOSTIC CHECKING STEP FOR THE ARIMA MODEL

par(mfrow=c(2,1))  # set up the graphics
hist(DAX.arima$residuals)
qqnorm(DAX.arima$residuals,main="Normal QQplot of ARIMA-DAX")      # normal Q-Q plot  
qqline(DAX.arima$residuals) 


par(mfrow=c(2,1))
acf(DAX.arima$residuals, 48)
pacf(DAX.arima$residuals, 48)
jarque.bera.test(DAX.arima$residuals)
shapiro.test(DAX.arima$residuals) 

XA.arima <- auto.arima(XA_Ret_ts)

#DIAGNOSTIC CHECKING STEP FOR THE ARIMA MODEL
par(mfrow=c(2,1))  # set up the graphics
hist(XA.arima$residuals)
qqnorm(XA.arima$residuals,main="Normal QQplot of ARIMA-XA")      # normal Q-Q plot  
qqline(XA.arima$residuals,col="blue") 


par(mfrow=c(2,1))
acf(XA.arima$residuals, 48)
pacf(XA.arima$residuals, 48)
jarque.bera.test(XA.arima$residuals)
shapiro.test(XA.arima$residuals)

LSE.arima <- auto.arima(LSE_Ret_ts)


par(mfrow=c(2,1))  # set up the graphics
hist(LSE.arima$residuals)
qqnorm(LSE.arima$residuals,main="Normal QQplot of ARIMA-XA")      # normal Q-Q plot  
qqline(LSE.arima$residuals,col="blue") 


par(mfrow=c(2,1))
acf(LSE.arima$residuals, 48)
pacf(LSE.arima$residuals, 48)
jarque.bera.test(LSE.arima$residuals)
shapiro.test(LSE.arima$residuals)
