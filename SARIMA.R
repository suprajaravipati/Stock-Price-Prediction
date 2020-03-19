library(openxlsx)
library(astsa)
# read in data on stock returns
new <- read.xlsx("Stock_Returns_1931_2002.xlsx")

# convert to timeseries object
data <- ts(new[, 3:4],start = c(1931, 1),end = c(2002, 12), frequency = 12)

# plot logarithm of dividend yield series
plot(data[, 2], 
     col = "steelblue", 
     lwd = 2, 
     ylab = "Logarithm", 
     main = "Dividend Yield for CRSP Index")

# EDA
x_trans <- data
acf(x_trans[,1])
pacf(x_trans[,1])
acf(x_trans[,2])
pacf(x_trans[,2])

#train test split
train <- data[1:692,1]
test  <- data[693:864,1]

#Model fit testing
sarima_model<-sarima(train,2,0,2,0,0,0,0)
#auto.arima(train,d = 2,D =2,max.p = 5,max.q = 5,max.P = 2,max.Q = 2)

#SARIMA
#One step ahead prediction
predictions=c()
for(i in (1:length(test)))
{
  y <- sarima.for(train, n.ahead=1, 2, 0, 2, P = 0, D = 0, Q = 0, S = 0, 
             tol = sqrt(.Machine$double.eps), 
             no.constant = TRUE,
             plot.all=FALSE, xreg = NULL, newxreg = NULL, fixed=NULL)
  train<-append(train,test[i],after=length(train))
  predictions<-append(predictions, y$pred[1], after = length(predictions))
  
}

#MSPE
sqrt(sum(((test-predictions)/test)^2)/length(test))

#Plot of predictions vs true data
pred_ts <- ts(predictions,start = c(1988, 9),end = c(2002, 12), frequency = 12)
plot(x_trans[,1],col="green")
lines(pred_ts,col="red")

predictions

#SARIMA with additional varibale
#Model fit testing
sarima(train,2,0,2,0,0,0,0,xreg = o_train)
#train test split
train <- data[1:692,1]
test  <- data[693:864,1]
o_train <- data[1:692,2]
o_test <- data[693:864,2]

#One step ahead prediction
predictions=c()
for(i in (1:length(test)))
{
  z <- sarima.for(train, n.ahead=1, 2, 0, 2, P = 0, D = 0, Q = 0, S = 0, 
                  tol = sqrt(.Machine$double.eps), 
                  no.constant = TRUE,
                  plot.all=FALSE, xreg = o_train, newxreg = o_test, fixed=NULL)
  train<-append(train,test[i],after=length(train))
  o_train<-append(o_train,o_test[i],after=length(o_train))
  predictions<-append(predictions, z$pred[1], after = length(predictions))
  
}

#MSPE
sqrt(sum(((test-predictions)/test)^2)/length(test))

#Plot of predictions vs true data
pred_ts <- ts(predictions,start = c(1988, 9),end = c(2002, 12), frequency = 12)
plot(x_trans[,1],col="green")
lines(pred_ts,col="red")

predictions