data = read.delim2(file="C:/Users/SAMMYA/Downloads/exercise6_220997078.txt", sep="", header=T)
library(bootstrap)
library(boot)



confidenceIntervalFunc = function(dataArr, lowerValue, upperValue) {
  return(c(quantile(dataArr, prob=lowerValue), quantile(dataArr, prob=upperValue)))
}

bootSize = 10000

######################## 6.1 ################

currVariable = (as.numeric(data$x1))
currVariableConfidenceInterval = bcanon(currVariable, nboot=bootSize, alpha=c(0.025, 0.975), theta=function(bootData){
  return(mean(bootData))
})

confInterval = currVariableConfidenceInterval$confpoints
confInterval[1,2]
confInterval[2,2]

###################### 6.2 ##################

resArr = vector(mode="numeric", length=bootSize)
for (x in 1:bootSize) {
  x_boot = sample(as.numeric(data$x1), replace=T)
  y_boot = sample(as.numeric(data$x2), replace=T)
  resArr[x] = mean(x_boot) - mean(y_boot)
}

bootResults =  boot(resArr, statistic=function(data, idx){
  return(mean(data[idx]))
}, R=bootSize)

bootResults

confidenceInterval <- confidenceIntervalFunc(resArr, lowerValue=0.025, upperValue=0.975)
confidenceInterval

############### 6.3 ###########################

rVals = vector(mode="numeric", length=bootSize)


for (y in 1:bootSize) {
  x1_boot = sample(as.numeric(data$x1), replace=T)
  x2_boot = sample(as.numeric(data$x2), replace=T)
  rVals[y] = sd(x1_boot) / sd(x2_boot)
}

rValsConfidenceInterval = confidenceIntervalFunc(rVals, lowerValue=0.025, upperValue=0.975)
rValsConfidenceInterval



################### 7.1 ######################

y_vals = as.numeric(data$y)

X1_mean = mean(as.numeric(data$x1))
X2_mean = mean(as.numeric(data$x2))
X3_mean = mean(as.numeric(data$x3))
X4_mean = mean(as.numeric(data$x4))
cat(X1_mean, X2_mean, X3_mean, X4_mean )

X_vals = as.numeric(data$x3)

model = lm(y_vals ~ X_vals)
summary(model)

model$coefficients

modelSlope = -0.0454
stdError = 0.09981

################### 7.2 ######################


betaHat = function(currData, X_vals, y_vals) {
  boot_data = currData[X_vals, y_vals]
  model = lm(y_vals ~ X_vals, data=boot_data)
  return(model$coefficients[2])
}


currData=data.frame(X_vals, y_vals)
n = length(X_vals)
lm_boot = boot(currData, statistic=betaHat, R=bootSize)


coEffs = vector(mode="numeric", length=bootSize)
for (x in 1:bootSize) {
  bootDataX = sample(X_vals, replace=T)
  bootDataY = sample(y_vals, replace=T)
  model =  lm(bootDataY ~ bootDataX)
  coEffs[x] = model$coefficients[2]
}


stdErrorCoeffs =  boot(coEffs, statistic=function(data, idx){
  return(mean(data[idx]))
}, R=bootSize)

stdErrorCoeffs

confidenceIntervalFunc(coEffs, lowerValue=0.025, upperValue=0.975)



################### 7.3 ######################


coEffVariableConfidenceInterval = bcanon(coEffs, nboot=bootSize, alpha=c(0.025, 0.975), theta=function(bootData){
  return(mean(bootData))
})


coEffVariableConfidenceInterval$confpoints


################### 7.4 ##########################


model = lm(y_vals ~ X_vals)
residualsVals = model$residuals
fittedVals = model$fitted


bootrepsInt = vector(mode="numeric", length=bootSize)
bootrepsSlope = vector(mode="numeric", length=bootSize)


for (idx in 1:bootSize) {
  yStar = fittedVals + sample(residualsVals, replace=T)
  mStar = lm(yStar ~ X_vals)
  bootrepsInt[idx] = mStar$coefficients[1]
  bootrepsSlope[idx] = mStar$coefficients[2]
}

slopeStdError = boot(bootrepsSlope, statistic=function(data, idx){
  return(mean(data[idx]))
}, R=bootSize)

slopeResidualConfidenceInterval = confidenceIntervalFunc(bootrepsSlope, lowerValue=0.025, upperValue=0.975)
slopeResidualConfidenceInterval


