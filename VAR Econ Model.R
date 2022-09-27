library("quantmod")  
library("tseries")  
library("vars")  
library("fxregime")  
library("tidyverse")
library(AppliedPredictiveModeling)
library(caret)
library(corrplot)
library(e1071)


df <- read.csv("GrowthData.csv")
df <- read.csv("Economy_norm2.csv")
#df <- read.csv("Economy_norm4.csv")
df <- read.csv("TotalVar_norm2.csv")


#Subset out days when consumer confidence is not missing (not == NA)

index.notNA.UM<-which(is.na(coredata(df$Consumer.Confidence.Index))==FALSE)
df.0<-df[index.notNA.UM,]
tail(df.0)

# set date
date.0 <- strptime(as.character(df.0$Date), "%m/%d/%Y")
date.0 <- format(date.0, "%Y-%m-%d")
df.0$Date.0 <- date.0

data <- xts(x=coredata(df.0), order.by=as.Date(df.0$Date.0))

data$Date <- NULL
data$Date.0 <- NULL
data$Initial.Claim <- NULL
head(data)

storage.mode(data) <- "numeric"
class(data)

acf(data$ISM)
adf.test(data$ISM)
plot(data$ISM)


plot(data[,2], main=dimnames(data)[[2]][1])

# VAR model no season
data.0 <- window(data, start = as.Date("1980-01-31"), 
                 end = as.Date("2022-07-31"))
sum(is.na(data.0))

data.0 <- na.omit(data.0)
head(data.0)
tail(data.0)

ISM <- data.0$ISM
CPI <- data.0$CPI
FF <- data.0$FEDFUNDS

X <- data.0
X$ISM<-NULL
X$CPI <-NULL

pcaObj <- prcomp(X, center = TRUE, scale. = TRUE)

X_pca <- pcaObj$x[,1:6]

X_pca <- xts(x=coredata(X_pca), order.by=as.Date(time(data.0)))


data.00 <- merge(FF, ISM, CPI,coredata(X_pca), all = TRUE)

 
acf(data.0$SP500)

# acf(as.data.frame(data), lag.max=10, na.action = na.pass)

data.0.VAR.const <- VARselect(data.0, lag.max=20,type="const")
data.0.VAR.const$selection

data.0.VAR.const.0<-VAR(data.0, p=data.0.VAR.const$selection[1],type="const")
options(show.signif.stars=FALSE)
summary(data.0.VAR.const.0)


VAR.pred <- predict(data.0.VAR.const.0, n.ahead = 9)
par(mar = c(0.1,0.1,0.1,0.1))
x11(); par(mai=rep(0.4, 4));plot(VAR.pred)
VAR.pred$fcst
plot(VAR.pred)

plot(irf(ymat00.0.VAR.const.0,impulse="UNRATE"))

# PCA prediction
pca.VAR <- VARselect(data.00, lag.max=20,type="const")
pca.VAR$selection

pca.VAR.0<-VAR(data.00, p=pca.VAR$selection[1],type="const")
options(show.signif.stars=FALSE)

summary(pca.VAR.0$varresult$ISM)
summary(pca.VAR.0$varresult$CPI)

acf(data.00, lag.max=10, na.action = na.pass, type = "partial")

# IRF
plot(irf(pca.VAR.0, impulse="FEDFUNDS", names = c("ISM", "CPI")))

pca.VAR.pred <- predict(pca.VAR.0, n.ahead = 9)
plot(pca.VAR.pred)
x11(); par(mai=rep(0.4, 4));plot(pca.VAR.pred, names = "CPI", xlim=c(455, 519));
x11(); par(mai=rep(0.4, 4));plot(pca.VAR.pred, names = "ISM", xlim=c(455, 519));

# plot
pca.VAR.pred$fcst$ISM
pca.VAR.pred$fcst$CPI

# plot 
x11(); par(mai=rep(0.4, 4));
fanchart(pca.VAR.pred, names = "CPI", main = "Fanchart for CPI", xlab = "Horizon", ylab = "CPI")
pca.VAR.pred


# VAR model with season
VAR.seaon <- VARselect(data.0, lag.max=20,type="const",season = 12)
VAR.seaon.0 <- VAR(data.0, p=VAR.seaon$selection[3],type="const",season = 12)
VAR.season.pred <- predict(data.0.VAR.const.0,n.ahead = 9)
summary(VAR.seaon.0)

plot(VAR.season.pred)
x11(); par(mai=rep(0.4, 4));plot(VAR.season.pred)


# VAR for diffed data
data.0.diff <- diff(as.matrix(data.0, lag = 1))

data.0.diff.VAR.const <- VARselect(data.0.diff, lag.max=20,type="const")
data.0.diff.VAR.const.0<-VAR(data.0.diff, p=data.0.diff.VAR.const$selection[3],type="const")

VAR.diff.pred <- predict(data.0.diff.VAR.const.0,n.ahead = 9, ci = 0.95)
summary(data.0.diff.VAR.const.0)
plot(VAR.diff.pred)

















