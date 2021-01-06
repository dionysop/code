library(xts)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(urca) 
library(rstatix)
library(fGarch)
library(forecast)
library(tseries)
library(dplyr)

###########################
#    Insert the data      #
###########################

#First the Data-Dataset
data_dates1_ts<- read.table("C:/Users/kez/Desktop/Safarikas_Time_Series/Data1/Data_dates1.txt",  header = T)
data_values1_ts<- read.table("C:/Users/kez/Desktop/Safarikas_Time_Series/Data1/Data_values1.txt",  header = T)
DAX <- cbind(data_dates1_ts, data_values1_ts) 
rm(data_dates1_ts, data_values1_ts)
DAX$DAX <- as.numeric(gsub(",", ".", DAX$DAX))


data_dates2_ts<- read.table("C:/Users/kez/Desktop/Safarikas_Time_Series/Data2/Data_dates2.txt",  header = T)
data_values2_ts<- read.table("C:/Users/kez/Desktop/Safarikas_Time_Series/Data2/Data_values2.txt",  header = T)
XA <- cbind(data_dates2_ts, data_values2_ts) 
rm(data_dates2_ts, data_values2_ts)
XA$FTSE.XA_LARGE_CAP <- as.numeric(gsub(",", ".", XA$FTSE.XA_LARGE_CAP))

data_dates3_ts<- read.table("C:/Users/kez/Desktop/Safarikas_Time_Series/Data3/Data_dates3.txt",  header = T)
data_values3_ts<- read.table("C:/Users/kez/Desktop/Safarikas_Time_Series/Data3/Data_values3.txt",  header = T)
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

summary(DAX_ts) # 51
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

#=============================================
# XA ARCH GARCH ANALYSIS
#=============================================

par(mfrow=c(2,2))

#=============================================
# Estimate ARCH(1) model - Normal distribution
#=============================================
XA1arch=garchFit(~garch(1,0),data=XA_Ret_ts,trace=F)       # trace = F   reduces the summary
summary(XA1arch) 
coef <- XA1arch@fit$coef
coef
fittedvar<- XA1arch@fit$series$h 

plot(XA1arch)
10
11
13
7
0
predict(XA1arch,6)

#================================================
# Estimate ARCH(1) model - Student-t distribution
#================================================
XA1archst=garchFit(~garch(1,0),data=XA_Ret_ts,cond.dist="std",trace=F) 
summary(XA1archst) 
plot(XA1archst)
10
11
13
7
0
predict(XA1archst,6)

#================================================
# Estimate GARCH(1,1) model - Normal distribution
#================================================
XA2garch = garchFit(~garch(1,1),data=XA_Ret_ts,trace=F) 
summary(XA2garch) 
plot(XA2garch)
10
11
13
7
0
predict(XA2garch,6)

#===================================================
# Estimate GARCH(1,1) model - Student-t distribution
#===================================================
XA2garchst=garchFit(~garch(1,1),data=XA_Ret_ts, cond.dist="std",trace=F) 
summary(XA2garchst) 
plot(XA2garchst)
10
11
13
7
0
predict(XA2garchst,6)

#===================================================
# Tring to combine this models ARMA & GARCH 
#===================================================

XA1armagarch=garchFit(~arma(0,1)+garch(1,1),data=XA_Ret_ts, cond.dist="std", trace=F)
summary(XA1armagarch)
plot(XA1armagarch)
10
11
13
7
0

forecast = predict(XA1armagarch,8)

predict(XA1armagarch,n.ahead=8,plot=TRUE,conf=.9,nx=100) 

#=============================================
# LSE ARCH GARCH ANALYSIS
#=============================================

par(mfrow=c(2,2))

#=============================================
# Estimate ARCH(1) model - Normal distribution
#=============================================
m1arch=garchFit(~garch(1,0),data=LSE_Ret_ts,trace=F)       # trace = F   reduces the summary
summary(m1arch) 
coef <- m1arch@fit$coef
coef
fittedvar<- m1arch@fit$series$h 

plot(m1arch)
13
0
predict(m1arch,6)

#================================================
# Estimate ARCH(1) model - Student-t distribution
#================================================
m1archst=garchFit(~garch(1,0),data=LSE_Ret_ts,cond.dist="std",trace=F) 
summary(m1archst) 
plot(m1archst)
13
0
predict(m1archst,6)


#================================================
# Estimate GARCH(1,1) model - Normal distribution
#================================================
m2garch=garchFit(~garch(1,1),data=LSE_Ret_ts,trace=F) 
summary(m2garch) 
plot(m2garch)
13
0
predict(m2garch,6)


#===================================================
# Estimate GARCH(1,1) model - Student-t distribution
#===================================================
m2garchst=garchFit(~garch(1,1),data=LSE_Ret_ts, cond.dist="std",trace=F) 
summary(m2garchst) 
plot(m2garchst)
13
0
predict(m2garchst,6)

#=============================================
# DAX ARCH GARCH ANALYSIS
#=============================================

par(mfrow=c(2,2))

#=============================================
# Estimate ARCH(1) model - Normal distribution
#=============================================
m1arch=garchFit(~garch(1,0),data=DAX_Ret_ts,trace=F)       # trace = F   reduces the summary
summary(m1arch) 
coef <- m1arch@fit$coef
coef
fittedvar<- m1arch@fit$series$h 

plot(m1arch)
13
0
predict(m1arch,6)

#================================================
# Estimate ARCH(1) model - Student-t distribution
#================================================
m1archst=garchFit(~garch(1,0),data=DAX_Ret_ts,cond.dist="std",trace=F) 
summary(m1archst) 
plot(m1archst)
13
0
predict(m1archst,6)



#================================================
# Estimate GARCH(1,1) model - Normal distribution
#================================================
m2garch=garchFit(~garch(1,1),data=DAX_Ret_ts,trace=F) 
summary(m2garch) 
plot(m2garch)
13
0
predict(m2garch,6)


#===================================================
# Estimate GARCH(1,1) model - Student-t distribution
#===================================================
m2garchst=garchFit(~garch(1,1),data=XA_DAX_Ret_ts, cond.dist="std",trace=F) 
summary(m2garchst) 
plot(m2garchst)
13
0
predict(m2garchst,6)
