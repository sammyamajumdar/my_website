# ### 1.a ###
# 
# pressFunction = function(data, columnFeature) {
#   rowsNum = 40
#   predictionVector = vector(length=rowsNum)
#   for(item in 1:rowsNum){
#     itemModel = lm(data$y ~ columnFeature, data=data[-item,])
#     predictionVector = predict(itemModel, newdata=data[item,])
#   }
#   pressValue = sum((data$y - predictionVector)^2)
#   
#   return (pressValue)
# }
# 
# covariatePressFunction = function(data, columnFeatureOne, columnFeatureTwo) {
#   rowsNum = 40
#   predictionVector = vector(length=rowsNum)
#   for(item in 1:rowsNum){
#     itemModel = lm(data$y ~ columnFeatureOne + columnFeatureTwo, data=data[-item,])
#     predictionVector = predict(itemModel, newdata=data[item,])
#   }
#   pressValue = sum((data$y - predictionVector)^2)
#   
#   return (pressValue)
# }
# 
# 
# 
# ## model 1 ##
# 
# modelOne = lm(workData$y ~ workData$x1)
# pressFunction(workData, workData$x1)
# 
# # ## model 2 ##
# 
# modelTwo = lm(workData$y ~ workData$x2)
# print(pressFunction(workData, workData$x2))
# #  
# # ## model 3 ##
# 
# modelOne = lm(workData$y ~ workData$x3)
# print(pressFunction(workData, workData$x3))
# 
# ### 1.b ###
# ## model 2 is the best performing model with the lowest PRESS statistic 
# 
# covariateModel = lm(workData$y ~ workData$x2+workData$x4)
# print(covariatePressFunction(workData, workData$x2,workData$x4))
# 
# ## adding x4 variable to model 2 increased model performance as PRESS statistic decreased from 336 to 334. 











########### Q9 ################


logLikelihoodFunction = function(y, pred) {
  return (y*(log(pred)) + ((1-y)*log(1-pred)))
}


logisticPressFunction = function(data, columnFeature) {
  rowsNum = 105
  predictionVector = vector(length=rowsNum)
  logLikelihoodVector = vector(length=rowsNum)
  for(item in 1:rowsNum){
    itemModel = glm(data$proc ~ columnFeature, data=data[-item,], family=binomial)
    predictionVector = predict(itemModel, newdata=data[item,], type="response")
    
    logLikelihoodVector = logLikelihoodFunction(data[,data$success],predictionVector)
  }
  predictiveValue = sum(logLikelihoodVector)

  return (predictiveValue)
}



covariateLogisticPressFunction = function(data, columnFeatureOne, columnFeatureTwo) {
  rowsNum = 105
  predictionVector = vector(length=rowsNum)
  logLikelihoodVector = vector(length=rowsNum)
  for(item in 1:rowsNum){
    itemModel = glm(data$proc ~ columnFeatureOne+columnFeatureTwo, data=data[-item,], family=binomial)
    predictionVector = predict(itemModel, newdata=data[item,], type="response")
    
    logLikelihoodVector = logLikelihoodFunction(data[,data$success],predictionVector)
  }
  predictiveValue = sum(logLikelihoodVector)
  
  return (predictiveValue)
}




## model 1 ##

logisticModelOne = glm(workDataTwo$success ~ workDataTwo$sex, family=binomial)
logisticPressFunction(workDataTwo, workDataTwo$sex)
## model 2 ##

logisticModelTwo = glm(workDataTwo$success ~ workDataTwo$age, family=binomial)
logisticPressFunction(workDataTwo, workDataTwo$age)
## model 3 ##

logisticModelThree = glm(workDataTwo$success ~ workDataTwo$smoke, family=binomial)
logisticPressFunction(workDataTwo, workDataTwo$smoke)


### 1.b ###
## model 3 has the highest predictive ability
logisticModelThree = glm(workDataTwo$success ~ workDataTwo$smoke+workDataTwo$proc, family=binomial)
covariateLogisticPressFunction(workDataTwo, workDataTwo$smoke,workDataTwo$proc)
