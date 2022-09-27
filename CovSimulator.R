library("quantmod")  
library("tseries")  
library("vars")  
library("fxregime") 
library("expm")

date.start<-'2020-01-01'
date.end<-'2021-05-31'

getSymbols("^GSPC", src = 'yahoo', from=date.start, to=date.end)

getSymbols("GE", src = 'yahoo',from=date.start, to=date.end)
getSymbols("BAC", src = 'yahoo',from=date.start, to=date.end)
getSymbols("XOM", src = 'yahoo',from=date.start, to=date.end)

testdata<-merge(diff(log(BAC$BAC.Close),lag=1),
                diff(log(GE$GE.Close),lag=1) ,
                  
                diff(log(XOM$XOM.Close),lag=1)  ,
                diff(log(GSPC$GSPC.Close), lag = 1)   )

write.csv(testdata, file = "testdata.csv")

testdata <- na.omit(testdata)
cov.mat <- cov(testdata)

e <- eigen(cov.mat)
e.val <- e$values
e.vec <- e$vectors

e.vec.T <- t(e.vec)
e.val.D <- diag(e.val)

e.val.D.sqrt <- sqrtm(e.val.D)

trans.mat <- e.val.D.sqrt%*%e.vec.T

norm.mat <-matrix(rnorm(100*4), 100, 4)

norm.mat%*%trans.mat
cov(norm.mat%*%trans.mat)
cov(testdata)

