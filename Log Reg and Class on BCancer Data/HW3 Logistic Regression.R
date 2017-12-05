#DAVA HW3 Logistic Regression
# Author: Daniel Rozen, drozen3

library(ggplot2)
library(Rcpp)
library(mlbench)
library(dplyr)

#Q2 Implement in R logistic regression based on gradient descent. To avoid unnecessary slowdown use vectorized code when computing the gradient (avoid loops).

# Input BreastCancer data
data(BreastCancer)

# take sample of df
df = BreastCancer[, ]
# remove NA entries
df = na.omit(df)

# convert Class column to benign = 0, malignant = 1
y = df$Class
y = lapply(y, as.character)

y = (lapply(y, function(x) {
  if (x == "benign") {
    return(-1)
  }
  else if (x == "malignant") {
    return(1)
  }
}))

y = as.numeric(y)

x = df[2:10]
x = cbind(x, rep(1, nrow(x))) # add intercept column to the sample
colnames(x)[10] = "intercept" # rename intercept column
#convert x to numeric matrix
x = as.matrix(sapply(x, as.numeric))

#initialize the dimensions of theta to random values
d = (ncol(x)[1])
theta = matrix(rexp(d, rate = 1) - .5, ncol = 1)

# create log reg function
logReg = function(x,
                  y,
                  theta,
                  alpha,
                  threshold,
                  decay = .99,
                  maxIter = 30000) {
  # Calculate new theta values with vectorized code:
  
  # prime for initial loop
  deltaUpdate = threshold
  iter = 0
  # loop until all update elements are smaller than threshold
  while (any(abs(deltaUpdate) >= threshold) && iter < maxIter) {
    z = (x) %*% (theta)
    A = y / (1 + exp(-1 * y * z))
    thetaOld = theta
    theta = theta - (alpha * t(x) %*% A)
    
    #decay alpha
    alpha = alpha * decay
    # delta value threshold
    deltaUpdate = (theta - thetaOld) / (abs(theta) + 0.0001)
    iter = iter + 1 #iteration count
  }
  cat("iterations:", iter)
  
  return(theta)
  
} # end logReg


# 3. 	Train and evaluate your code on the BreastCancer data from the mlbench R package.
#Specifically, randomly divide the dataset into 70% for training and 30% for testing (see pg 23 in notes3)

r = nrow(x)

# loop evaluation code
repetitions = 10

N = seq(1, repetitions)

errorPercentageTrainList = c()
errorPercentageList = c()

while (repetitions > 0) {
  rand_perm = sample(r, r)
  first_set_of_indices = rand_perm[1:floor(r * 0.7)]
  second_set_of_indices = rand_perm[(floor(r * 0.7) + 1):r]
  trainX = x[first_set_of_indices,]
  testX = x[second_set_of_indices,]
  
  trainY = y[first_set_of_indices]
  testY = y[second_set_of_indices]
  
  #and train on the training set
  theta = matrix(rexp(d, rate = 1) - .5, ncol = 1)
  theta = logReg(
    trainX,
    trainY,
    theta,
    alpha = .01,
    decay = 0.99,
    threshold = 1e-8,
    maxIter = 30000
  )
  
  # take only the sign of y
  takeSign = function(x) {
    if (x > 0) {
      return(-1)
    }
    else if (x < 0) {
      return(1)
    }
    else if (x == 0) {
      return(0)
    }
  }
  
  yTrainResult = as.numeric(lapply((trainX %*% theta), takeSign))
  yTestResult = as.numeric(lapply((testX %*% theta), takeSign))
  
  yTDiff = trainY - yTrainResult
  errorPercentageTrain = 100 * sum(yTDiff != 0) / length(yTDiff)
  
  #and report your accuracy (fraction of times the model made a mistake) on the train set and on the test set.
  yDiff = testY - yTestResult
  errorPercentage = 100 * sum(yDiff != 0) / length(yDiff)
  
  errorPercentageTrainList = c(errorPercentageTrainList , errorPercentageTrain)
  errorPercentageList = c(errorPercentageList , errorPercentage)
  
  repetitions = repetitions - 1
} # end while loop

dfResults = data.frame(N = N,
                       trainingError = errorPercentageTrainList,
                       testError = errorPercentageList)
dfResults

#Repeat the random partition of 70% and 30% 10 times and average the test accuracy results over the 10 repetitions.

AverageTrainError = mean(dfResults$trainingError)
AverageTestError = mean(dfResults$testError)

AverageTrainError
AverageTestError

sdTrainError = sd(dfResults$trainingError)
sdTestError = sd(dfResults$testError)
sdTrainError
sdTestError

# plot run time as a function of array size for R and
# .C implementations

ggplot(dfResults, aes(N)) +
  geom_line(aes(y = trainingError, color = "trainingError")) +
  geom_line(aes(y = testError, color = "testError")) +
  xlab("Sample # N") +
  ylab("error (%)") +
  ggtitle("Training and Testing Error")


# 4.   Repeat (3) but this time using logistic regression training code from an R package such as glm2. How did the accuracy in (4) compare to the accuracy in (3).
#
library(glm2)

r = nrow(x)

# loop evaluation code
repetitions = 10
N = seq(1, repetitions)

errorTrainListGLM = c()
errorTestListGLM = c()
# take y to be {0,1} for glm purposes

y0 = as.numeric(df$Class) - 1

while (repetitions > 0) {
  rand_perm = sample(r, r)
  first_set_of_indices = rand_perm[1:floor(r * 0.7)]
  second_set_of_indices = rand_perm[(floor(r * 0.7) + 1):r]
  trainX = x[first_set_of_indices, ]
  testX = x[second_set_of_indices, ]
  
  trainY = y0[first_set_of_indices]
  testY = y0[second_set_of_indices]
  
  #and train on the training set
  
  dfTrain = as.data.frame(cbind(trainX[, 1:9], trainY))
  dfTest = as.data.frame(cbind(testX[, 1:9], testY))
  
  glmFit = glm2(
    trainY ~ .,
    data = dfTrain,
    family = binomial(link = "logit"),
    control = glm.control(maxit = 300)
  )
  
  ## report accuracy
  
  glmTrainAcc = mean(dfTrain$trainY == (predict.glm(
    glmFit, newdata = dfTrain, type = "response"
  ) >= 0.5))
  glmTrainErrorRate = (1 - glmTrainAcc) * 100
  
  glmTestAcc = mean(dfTest$testY == (predict.glm(
    glmFit, newdata = dfTest, type = "response"
  ) >= 0.5))
  glmTestErrorRate = (1 - glmTestAcc) * 100
  
  errorTrainListGLM = c(errorTrainListGLM, glmTrainErrorRate)
  errorTestListGLM = c(errorTestListGLM, glmTestErrorRate)
  
  repetitions = repetitions - 1
} # end while loop

glmResultsDF = data.frame(N = N,
                          trainError = errorTrainListGLM,
                          testError = errorTestListGLM)
glmResultsDF

#Repeat the random partition of 70% and 30% 10 times and average the test accuracy results over the 10 repetitions.

GLMAverageTrainError = mean(glmResultsDF$trainError)
GLMsdTrainError = sd(glmResultsDF$trainError)

GLMAverageTestError = mean(glmResultsDF$testError)
GLMsdTestError = sd(glmResultsDF$testError)

GLMAverageTrainError
GLMsdTrainError

GLMAverageTestError
GLMsdTestError
# plot run time as a function of array size 

ggplot(glmResultsDF, aes(N)) +
  geom_line(aes(y = trainError, color = "trainError")) +
  geom_line(aes(y = testError, color = "testError")) +
  
  xlab("Sample # N") +
  ylab("error (%)") +
  ggtitle("Training and Testing Error for GLM")



# 5 	Repeat (4), but replace the 70%-30% train-test split with each of the following splits: 5%-95%,10%-90%, ., 95%-5%. :

# increment the split by 5% each time and average the accuracy over 10 random divisions of data for the same split size

r = nrow(x)

# loop evaluation code

# take y to be {0,1} for glm purposes

y0 = as.numeric(df$Class) - 1

errorAvTrainListGLM = c()
errorAvTestListGLM = c()

#train-test split variable
trainSeq = seq(5, 95, length = 19)

for (splitVar in trainSeq) {
  cat("\n\n split: ", splitVar, "%, ", 100 - splitVar, "%")
  
  errorTrainListGLM = c()
  errorTestListGLM = c()
  
  repetitions = 10
  N = seq(1, repetitions)
  
  while (repetitions > 0) {
    rand_perm = sample(r, r)
    first_set_of_indices = rand_perm[1:floor(r * splitVar / 100)]
    second_set_of_indices = rand_perm[(floor(r * splitVar / 100) + 1):r]
    trainX = x[first_set_of_indices, ]
    testX = x[second_set_of_indices, ]
    
    trainY = y0[first_set_of_indices]
    testY = y0[second_set_of_indices]
    
    #and train on the training set
    
    dfTrain = as.data.frame(cbind(trainX[, 1:9], trainY))
    dfTest = as.data.frame(cbind(testX[, 1:9], testY))
    
    glmFit = glm2(
      trainY ~ .,
      data = dfTrain,
      family = binomial(link = "logit"),
      control = glm.control(maxit = 300)
    )
    
    ## report accuracy
    
    glmTrainAcc = mean(dfTrain$trainY == (
      predict.glm(glmFit, newdata = dfTrain, type = "response") >= 0.5
    ))
    glmTrainErrorRate = (1 - glmTrainAcc) * 100
    
    glmTestAcc = mean(dfTest$testY == (
      predict.glm(glmFit, newdata = dfTest, type = "response") >= 0.5
    ))
    glmTestErrorRate = (1 - glmTestAcc) * 100
    
    errorTrainListGLM = c(errorTrainListGLM, glmTrainErrorRate)
    errorTestListGLM = c(errorTestListGLM, glmTestErrorRate)
    
    repetitions = repetitions - 1
  } # end while loop
  
  glmResultsDF = data.frame(N = N,
                            trainError = errorTrainListGLM,
                            testError = errorTestListGLM)
  glmResultsDF
  #Repeat the random partition of 70% and 30% 10 times and average the test accuracy results over the 10 repetitions.
  
  GLMAverageTrainError = mean(glmResultsDF$trainError)
  GLMsdTrainError = sd(glmResultsDF$trainError)
  
  GLMAverageTestError = mean(glmResultsDF$testError)
  GLMsdTestError = sd(glmResultsDF$testError)
  
  errorAvTrainListGLM = c(errorAvTrainListGLM, GLMAverageTrainError)
  errorAvTestListGLM = c(errorAvTestListGLM, GLMAverageTestError)
  
  cat("\nGLMAverageTrainError:" , GLMAverageTrainError)
  cat("\nGLMAverageTestError:" , GLMAverageTestError)
  
} # end for loop

glmAvResultsDF = data.frame(trainSetSize = trainSeq,
                            trainError = errorAvTrainListGLM,
                            testError = errorAvTestListGLM)
glmAvResultsDF


#Graph the accuracy over the training set and over the testing set as a function of the size of the train set. Remember to average the accuracy over 10 random divisions of the data into train and test sets of the above sizes so the graphs will be less noisy.
#

ggplot(glmAvResultsDF, aes(trainSetSize)) +
  geom_line(aes(y = trainError, color = "trainError")) +
  geom_line(aes(y = testError, color = "testError")) +
  xlab("Train Set Size (%)") +
  ylab("error (%)") +
  ggtitle("Average Training and Testing Error for GLM vs. Train Set Size")


#
# 6. 	Repeat (5) but instead of graphing the train and test accuracy, graph the logistic regression loss function (negative log likelihood) over the train set and over the test set as a function of the train set size.
# please normalize the loss function by the size of the data you are measuring (divide by n - the number of samples in your evaluation set). This is necessarily to make the loss function or loglikelihood numbers consistent across different sample sizes

r = nrow(x)

# loop evaluation code

# take y to be {0,1} for glm purposes

y0 = as.numeric(df$Class) - 1

lossFunAveTrainList = c()
lossFunAveTestList = c()

#train-test split variable
trainSeq = seq(5, 95, length = 19)

for (splitVar in trainSeq) {
  cat("\n\n split: ", splitVar, "%, ", 100 - splitVar, "%")
  
  lossFunTrainList = c()
  lossFunTestList = c()
  
  repetitions = 10
  
  N = seq(1, repetitions)
  
  set.seed(123)
  
  while (repetitions > 0) {
    rand_perm = sample(r, r)
    first_set_of_indices = rand_perm[1:floor(r * splitVar / 100)]
    second_set_of_indices = rand_perm[(floor(r * splitVar / 100) + 1):r]
    trainX = x[first_set_of_indices, ]
    testX = x[second_set_of_indices, ]
    
    trainY = as.matrix(y0[first_set_of_indices])
    testY = as.matrix(y0[second_set_of_indices])
    
    trainYLoss = as.matrix(y[first_set_of_indices])
    testYLoss = as.matrix(y[second_set_of_indices])
    
    #and train on the training set
    
    dfTrain = as.data.frame(cbind(trainX[, 1:9], trainY))
    colnames(dfTrain)[10] = "trainY"
    dfTest = as.data.frame(cbind(testX[, 1:9], testY))
    colnames(dfTest)[10] = "testY"
    
    ## calculate loss function
    lossFun = function(x, y, theta) {
      lossFunTotal = sum(-1 * log(1 + exp(-1 * y * (x %*% theta)))) / nrow(x)
      return(lossFunTotal)
    }
  
    #use glm
    glmFit = glm2(
      trainY ~ .,
      data = dfTrain,
      family = binomial(link = "logit"),
      control = glm.control(maxit = 50)
    )
    
    glmTheta = as.matrix(unlist(glmFit[1]))
    # swap intercept to the 10th position
    swap = glmTheta[1]
    glmTheta[1] = glmTheta[10]
    glmTheta[10] = swap
    
    lossFunTrain = -logLik(glmFit) / nrow(trainX)
    
    lossFunTest = -(sum(log(
      predict.glm(glmFit, newdata = dfTest[dfTest$testY == 1, 1:9], type = "response")
    )) +
      sum(log(
        predict.glm(glmFit, newdata = dfTest[dfTest$testY == 0, 1:9], type = "response")
      ))) / nrow(testX)
    
    lossFunTrainList = c(lossFunTrainList, lossFunTrain)
    lossFunTestList = c(lossFunTestList, lossFunTest)
    
    repetitions = repetitions - 1
  } # end while loop
  
  lossFunResultsDF = data.frame(N = N,
                                lossFunTrain = lossFunTrainList,
                                lossFunTest = lossFunTestList)
  
  #Repeat the random partition of 70% and 30% 10 times and average the test accuracy results over the 10 repetitions.
  
  lossFunAverageTrain = mean(lossFunResultsDF$lossFunTrain)
  lossFunAverageTest = mean(lossFunResultsDF$lossFunTest)
  
  lossFunAveTrainList = c(lossFunAveTrainList, lossFunAverageTrain)
  lossFunAveTestList = c(lossFunAveTestList, lossFunAverageTest)
  # plot run time as a function of array size for R and
  # .C implementations
  
  cat("\nlossFunAverageTrain:" , lossFunAverageTrain)
  cat("\nlossFunAverageTest:" , lossFunAverageTest)
  
} # end for loop

glmAvResultsDF = data.frame(
  trainSetSize = trainSeq,
  lossFunTrain = lossFunAveTrainList,
  lossFunTest = lossFunAveTestList
)
glmAvResultsDF


#Graph the accuracy over the training set and over the testing set as a function of the size of the train set. Remember to average the accuracy over 10 random divisions of the data into train and test sets of the above sizes so the graphs will be less noisy.
#

ggplot(glmAvResultsDF, aes(trainSetSize)) +
  geom_line(aes(y = lossFunTrain, color = "Training Loss Function")) +
  geom_line(aes(y = lossFunTest, color = "Testing Loss Function")) +
  xlab("Train Set Size (%)") +
  ylab("Logistic Regression Loss Function") +
  ggtitle("Average Logistic Regression Loss Function vs. Train Set Size")
