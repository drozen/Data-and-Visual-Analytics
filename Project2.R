#Project2
# Author: Daniel Rozen, drozen3

library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)

load('movies_merged')
# create copy to modify
movieOnly=data.frame(movies_merged)

#Remove all rows that do not correspond to movies.
movieOnly= subset(movieOnly, Type == "movie")

# omit all rows in which gross and budget are not available.
# remove Gross NA columns
#convert "N/A" to NA
movieOnly[movieOnly == "N/A"] = NA

movieOnly = movieOnly[!is.na(movieOnly$Gross),]
movieOnly = movieOnly[!is.na(movieOnly$Budget),]

#remove all movies released prior to 2000:

movieOnly = movieOnly[(movieOnly$Year)>=2000,]

# From Project 1

#Write R code to convert Runtime to a numeric value (in minutes) 

#create new dataframe to copy Runtime to
runTime=data.frame(movieOnly$Runtime)

# separate runtime column into minutes and min
runTimeSep = separate(runTime, "movieOnly.Runtime", c("hours", "h", "minutes", "min"), " ")

hoursCol = as.numeric(runTimeSep$hours) # extract the hours digits and convert to numeric values
runTimeSep$hours = hoursCol

minutesCol = as.numeric(runTimeSep$minutes) # extract the minute digits and convert to numeric values
runTimeSep$minutes = minutesCol

#convert hours to minutes
runTimeSep$newMins = with(runTimeSep, (h=="h")*60*hours + (h=="min")*hours + ifelse(is.na(minutes), 0, minutes))

movieOnly$Runtime = as.numeric(runTimeSep$newMins) #replace runtime with  the new numeric column

# remove unneeded Data
remove(minutesCol)
remove(hoursCol)
remove(awardSumDF)


# From Project 1 Code: Convert Awards to a numeric column as a total # of awards and nominations

#create new dataframe to copy awardsDF to
library(stringr)

# extract numbers
inputX = movieOnly$Awards
x = (regmatches(inputX, gregexpr('\\(?[0-9]+', inputX)))
x = lapply(x, as.numeric) # convert to numeric
xsum = lapply(x, sum) # sum up each row

awardSumDF = data.frame(unlist(xsum))

remove(xsum)
# rename columns
awardSumDF = plyr::rename(awardSumDF, c("unlist.xsum." = "awardSum"))

movieOnly = dplyr::bind_cols(movieOnly, awardSumDF) # merge dataframes

# Drop columns we don't need:
dropList = c('Gross', 'Domestic_Gross', 'BoxOffice', 'imdbID', 'Plot', 'Poster', 'Website', 'tomatoURL', 'tomatoConsensus', 'Response', 'Type')

# function to drop column names in x
drop = function(df, x) {
  return(df[ , !(names(df) %in% x)])
}

movieDrop = drop(movieOnly, dropList)

#convert "N/A" to NA
movieDrop[movieDrop == "N/A"] = NA

#convert metascore to numeric

movieDrop$Metascore = as.numeric(movieDrop$Metascore)

# MSE function
# input: the output of lm, eg. M1 = lm(price~carat, diamSmall);

MSEfun = function(M1) {
  return(mean(residuals(M1)^2))
}

# predict the profit associated with a movie, defined as gross revenue minus budget. 
# calculate profit
y = as.data.frame(as.numeric(movieOnly$Gross - movieOnly$Budget))
colnames(y)[1] = "Profit" # rename column name

x = movieDrop

remove(movies_merged)

# The following code is adapted from HW3:


############ RUN EVERY STEP TILL HERE ##############

################Q1################

# 1. Use linear regression to predict profit based on all available numeric variables. Graph the train and test MSE as a function of the train set size (averaged over 10 random data partitions as described above)?
r = nrow(x)

MSEAvTrainList = c()
MSEAvTestList = c()

#train-test split variable
trainSeq = seq(5, 95, length = 19)

for (splitVar in trainSeq) {
  cat("\n\n split: ", splitVar, "%, ", 100 - splitVar, "%\n\n")
  
  MSEtrainList = c()
  MSEtestList = c()
  
  repetitions = 30 #############ADJUST REPETITIONS TO 10 ################
  
  N = seq(1, repetitions)
  
  while (repetitions > 0) {
    
    #Randomly divide the rows into two sets of sizes 5% and 95%
    
    rand_perm = sample(r, r)
    first_set_of_indices = rand_perm[1:floor(r * splitVar / 100)]
    second_set_of_indices = rand_perm[(floor(r * splitVar / 100) + 1):r]
    trainX = x[first_set_of_indices, ]
    testX = x[second_set_of_indices, ]
    
    trainY = y[first_set_of_indices,]
    testY = y[second_set_of_indices,]
    
    #and train on the training set
    
    dfTrain = as.data.frame(cbind(trainX, trainY))
    colnames(dfTrain)[length(dfTrain)] = "Profit"
    dfTest = as.data.frame(cbind(testX, testY))
    
    # Compute the MSE on the train and test sets (normalize the MSE by the number of samples)
    
    # 1. Use linear regression to predict profit based on all available numeric variables. Graph the train and test MSE as a function of the train set size (averaged over 10 random data partitions as described above)?
    
    dfTrain1 = select(dfTrain, Runtime , awardSum , Metascore , imdbRating , imdbVotes , tomatoMeter , tomatoRating , tomatoFresh , tomatoRotten , tomatoUserMeter , tomatoUserReviews, Budget, Profit)
    
    dfTrain1 = na.omit(dfTrain1)
    
    dfTest1 = select(dfTest, Runtime , awardSum , Metascore , imdbRating , imdbVotes , tomatoMeter , tomatoRating , tomatoFresh , tomatoRotten , tomatoUserMeter , tomatoUserReviews, Budget, testY)
    
    dfTest1 = na.omit(dfTest1)
    # ensure testY has the same number of datapoints as testX
    
    testY = dfTest1$testY
    dfTest1 = drop(dfTest1, 'testY')     # remove testY from dfTest1
    
    M1 = lm(Profit~Runtime + awardSum + Metascore + imdbRating + imdbVotes + tomatoMeter + tomatoRating + tomatoFresh + tomatoRotten + tomatoUserMeter +  tomatoUserReviews + Budget, dfTrain1);
    theta<-coef(M1)
    
    MSEtrain = MSEfun(M1)
    
    myPred = predict(M1, dfTest1)
    
    MSEtest = mean((myPred - testY)^2)
    
    MSEtrainList = c(MSEtrainList, MSEtrain)
    MSEtestList = c(MSEtestList, MSEtest)
    
    repetitions = repetitions - 1
  } # end while loop
  
  MSEdf = data.frame(N = N,
                     trainError = MSEtrainList,
                     testError = MSEtestList)
  MSEdf
  #Repeat the random partition of 70% and 30% 10 times and average the test accuracy results over the 10 repetitions.
  
  MSEAvTrainList = c(MSEAvTrainList, mean(MSEdf$trainError))
  MSEAvTestList = c(MSEAvTestList, mean(MSEdf$testError))
  
} # end for loop

MSEAvDF = data.frame(trainSetSize = trunc(trainSeq * r/100),
                     trainMSE = MSEAvTrainList,
                     testMSE = MSEAvTestList)
MSEAvDF


#Graph the accuracy over the training set and over the testing set as a function of the size of the train set. Remember to average the accuracy over 10 random divisions of the data into train and test sets of the above sizes so the graphs will be less noisy.
#


# - Generate a graph of the averaged train and test MSE as a function of the train set size

ggplot(MSEAvDF, aes(trainSetSize)) +
  geom_line(aes(y = trainMSE, color = "trainMSE")) +
  geom_line(aes(y = testMSE, color = "testMSE")) +
  xlab("Train Set Size") +
  ylab("MSE") +
  ggtitle("Q1: Average Training and Testing MSE vs. Train Set Size")


####################################################################################

#2. 2.	Try to improve the prediction quality in (1) as much as possible by adding feature transformations of the numeric variables. Explore both numeric transformations such as power transforms and non-numeric transformations of the numeric variables like binning (e.g., is_budget_greater_than_3M). Explain which transformations you used and why you chose them. Graph the train and test MSE as a function of the train set size (averaged over 10 random data partitions as described above)? 

# correlation research from Proj1

#investigate the pairwise relationships between these different descriptors using graphs. 
library(GGally)
# select columns to use with GG pairs

movieDrop=drop(movieDrop, 'Profit')

movieDropGG = as.data.frame(cbind(movieDrop, y))

ggpairsDF = select(movieDropGG, imdbRating, imdbVotes, tomatoFresh, awardSum, Profit) 

# rename columns for easier graph readability
# ggpairsDF = plyr::rename(ggpairsDF, c("imdbRating" = "imdbRat", "tomatoMeter"="tomatoMet", "tomatoRating" = "tomatoRat", "tomatoUserMeter"= "tomUserMet", "tomatoUserRating" = "tomUseRat" ))
# names(ggpairsDF)

ggcorr(ggpairsDF, method="na.or.complete")
ggpairs(ggpairsDF)


########### VISUALIZATION EXPLORATION #############

#plot the scatterplot
ggplot(ggpairsDF,aes((imdbRating),(Profit))) + geom_point()

# with line fit
qplot(imdbRating, #x
      Profit,  #y
      data = ggpairsDF,
      main = "Profit vs imdbRating") +
  stat_smooth(method = "loess",
              method.args = list(degree = 0),
              span = 0.4,
              se = FALSE) + 
  coord_cartesian(ylim = c(0,500000000))


# log plot
ggplot(ggpairsDF,aes(log(imdbRating),log(Profit))) + geom_point()


######Use binning from Proj1 ##################################

convertFun = function(x) {
  # upperThreshold = meanAwards + sdAwards*.5
  # set thresholds
  t1=60
  t2=140
  if (x == 0) {x = "none"} 
  else if (x >=1 & x<=t1) {x="some"}
  else if (x >t1 & x<=t2) {x="many"}
  else if (x >t2 ) {x="hundreds"}
}

xsum = x$awardSum
categorized = lapply(xsum, convertFun)

categorizedDF = data.frame(unlist(categorized))
# rename columns
categorizedDF = plyr::rename(categorizedDF, c("unlist.categorized."="awardCategory"))


x = dplyr::bind_cols(x, categorizedDF) # merge dataframes
xy = dplyr::bind_cols(x, y) # merge dataframes
ggplot(xy, aes(reorder(awardCategory, -Profit, median), Profit)) +
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete("Category")

#zoom in

ggplot(xy, aes(reorder(awardCategory, -Profit, median), Profit)) +
  geom_boxplot() +
  coord_flip(ylim = c(0,1000000000)) +
  scale_x_discrete("Category")

################# BEGIN Q2 RUN FROM HERE ####################

r = nrow(x)

MSEAvTrainList = c()
MSEAvTestList = c()
MSEAvTrainList2 = c()
MSEAvTestList2 = c()

#train-test split variable
#trainSeq = seq(5, 95, length = 19)

# shorter trainSeq
trainSeq = trunc(seq(5, 95, length = 19))

for (splitVar in trainSeq) {
  cat("\n\n split: ", splitVar, "%, ", 100 - splitVar, "%\n\n")
  
  MSEtrainList = c()
  MSEtestList = c()
  MSEtrainList2 = c()
  MSEtestList2 = c()
  
  repetitions = 30 #############ADJUST REPETITIONS TO 10 ################
  
  N = seq(1, repetitions)
  
  while (repetitions > 0) {
    
    #Randomly divide the rows into two sets of sizes 5% and 95%
    
    rand_perm = sample(r, r)
    first_set_of_indices = rand_perm[1:floor(r * splitVar / 100)]
    second_set_of_indices = rand_perm[(floor(r * splitVar / 100) + 1):r]
    trainX = x[first_set_of_indices, ]
    testX = x[second_set_of_indices, ]
    
    trainY = y[first_set_of_indices,]
    testY = y[second_set_of_indices,]
    
    #and train on the training set
    
    dfTrain = as.data.frame(cbind(trainX, trainY))
    colnames(dfTrain)[length(dfTrain)] = "Profit"
    dfTest = as.data.frame(cbind(testX, testY))
    
    # Compute the MSE on the train and test sets (normalize the MSE by the number of samples)
    
    #columns to use in lm
    lmColNames = paste(names(dfTest), collapse=' + ')
  
    
    # 1. Use linear regression to predict profit based on all available numeric variables. Graph the train and test MSE as a function of the train set size (averaged over 10 random data partitions as described above)?
    
    dfTrain1 = select(dfTrain, Runtime , awardSum , awardCategory, Metascore , imdbRating , imdbVotes , tomatoMeter , tomatoRating , tomatoFresh , tomatoRotten , tomatoUserMeter , tomatoUserReviews, Budget, Profit)
    
    dfTrain1 = na.omit(dfTrain1)
    
    dfTest1 = select(dfTest, Runtime , awardSum , awardCategory, Metascore , imdbRating , imdbVotes , tomatoMeter , tomatoRating , tomatoFresh , tomatoRotten , tomatoUserMeter , tomatoUserReviews, Budget, testY)
    
    dfTest1 = na.omit(dfTest1)
    # ensure testY has the same number of datapoints as testX
    testY = dfTest1$testY
    dfTest1 = drop(dfTest1, 'testY')     # remove testY from dfTest1
    
    # Orginal Model
    
    M1 = lm(Profit~Runtime + awardSum + Metascore + imdbRating + imdbVotes + tomatoMeter + tomatoRating + tomatoFresh + tomatoRotten + tomatoUserMeter +  tomatoUserReviews + Budget, dfTrain1);
    
    ################################## ADJUST MODEL HERE #####################################

    # Using binning
    
    M2 = lm(Profit~Runtime + awardSum * Metascore * imdbRating * imdbVotes + tomatoMeter + tomatoRating + tomatoFresh + tomatoRotten + tomatoUserMeter + Budget + I(awardSum^2) + I(awardSum^3) + I(imdbVotes^2) + I(imdbVotes^3) +  I(tomatoFresh^2) + I(tomatoFresh^3) + I(imdbRating^2) + I(imdbRating^3) + awardCategory, dfTrain1);
    
    theta<-coef(M1)
    
    #summary(M1)$r.squared
    
    MSEtrain = MSEfun(M1)
    MSEtrain2 = MSEfun(M2)
    
    MSEtest = mean((predict(M1, dfTest1) - testY)^2)
    MSEtest2 = mean((predict(M2, dfTest1) - testY)^2)
    
    MSEtrainList = c(MSEtrainList, MSEtrain)
    MSEtestList = c(MSEtestList, MSEtest)
    
    MSEtrainList2 = c(MSEtrainList2, MSEtrain2)
    MSEtestList2 = c(MSEtestList2, MSEtest2)
    
    repetitions = repetitions - 1
  } # end while loop
  
  MSEdf = data.frame(N = N,
                     trainError = MSEtrainList,
                     testError = MSEtestList,
                     trainError2 = MSEtrainList2,
                     testError2 = MSEtestList2)
  print(colMeans(MSEdf))
  #Repeat the random partition of 70% and 30% 10 times and average the test accuracy results over the 10 repetitions.
  
  MSEAvTrainList = c(MSEAvTrainList, mean(MSEdf$trainError))
  MSEAvTestList = c(MSEAvTestList, mean(MSEdf$testError))
  MSEAvTrainList2 = c(MSEAvTrainList2, mean(MSEdf$trainError2))
  MSEAvTestList2 = c(MSEAvTestList2, mean(MSEdf$testError2))
  
} # end for loop

MSEAvDF = data.frame(trainSetSize = trunc(trainSeq * r/100),
                     trainMSE = MSEAvTrainList,
                     testMSE = MSEAvTestList,
                     trainMSE2 = MSEAvTrainList2,
                     testMSE2 = MSEAvTestList2)
MSEAvDF


#Graph the accuracy over the training set and over the testing set as a function of the size of the train set. Remember to average the accuracy over 10 random divisions of the data into train and test sets of the above sizes so the graphs will be less noisy.
#

# - Generate a graph of the averaged train and test MSE as a function of the train set size

ggplot(MSEAvDF, aes(trainSetSize)) +
  geom_line(aes(y = trainMSE, color = "OldTrainMSE")) +
  geom_line(aes(y = testMSE, color = "OldTestMSE")) +
  geom_line(aes(y = trainMSE2, color = "NewTrainMSE")) +
  geom_line(aes(y = testMSE2, color = "NewTestMSE")) +
  xlab("Train Set Size") +
  ylab("MSE") +
  ggtitle("Q2 Average Training and Testing MSE vs. Train Set Size") +
  coord_cartesian(ylim = c(5e+15,1.5e+16))

############## Plotting residuals ################################################

residDF = as.data.frame(summary(M1)$residuals)

qplot(x= dfTrain1$Runtime,
      y= summary(M1)$residuals,
      data = residDF,
      main = "figure title",
      geom = "point")

####################################################################################


#############      Q3	   ##############################################
#Write code that featurizes genre (can use code from Part-I), actors, directors, and other categorical variables. Explain how you encoded the variables into features



########## Use Code from Damien's Lasso post###############

### Code from Classmate Damien Beneviste posted on Piazza @810
# function to remove some not very useful levels
collapseCat <-
  function(column,
           columnNameText,
           threshold = 0.001,
           excludeArg = NA) {
    tempCol = as.character(column)
    tempTable = table(tempCol, exclude = excludeArg)
    tempPropTable = prop.table(tempTable)
    tempCollapse = names(which(tempPropTable < threshold))
    if (length(tempCollapse) <= 1) {
      return(column)
    } else {
      tempOut = tempCol
      tempOut[tempOut %in% tempCollapse] = paste0("Collapse_", columnNameText)
      return(tempOut)
    }
  }

xy = bind_cols(x, y) 
# moviesForBin = xy[c("Title", "Profit", "Director", "Actors", "Writer", "Rated", "Language", "Production", "Country")]
moviesForBin = (xy[c("Title", "Profit", "Director", "Actors", "Writer", "Production", "Genre", "Rated", "Language", "Country")])
moviesForBin[moviesForBin == "N/A"] = NA
moviesForBin = na.omit(moviesForBin)

#create the dummy columns (one-hot encoding)

library(tidyr)
library(reshape2)

# This function converts categorical variables to binary vectors using one-hot encoding
toDummyVars <- function(df, colName, thresh = 0.005) {  
  dfTemp = df
  dfTemp$colName <- strsplit(dfTemp[,colName],"(\\s)?,(\\s)?")  # separate by commas
  unnested <- unnest(dfTemp) # convert to long format with each feature separated with correct corresponding Profit.  
  unnested$colName <- gsub("\\s","_",unnested$colName)  # replace spaces with _
  unnested$colName = chartr("/αισ-", "_aeo_",unnested$colName)
  unnested$colName <- gsub('"',"",unnested$colName , fixed="TRUE")
  unnested$colName <- gsub("(","",unnested$colName, fixed=TRUE)
  unnested$colName <- gsub(")","",unnested$colName, fixed=TRUE)
  # remove categories which are of proportion < .0001 for feature selection
  unnested$colName <- collapseCat(unnested$colName, colName, threshold = paste(thresh))
  dfTemp <- dcast(unnested, Title + Profit ~ `colName`, fun.aggregate = length, value.var = "colName")  # convert to binary vectors with one-hot encoding.  
  return(dfTemp)
}

#old versions
# This function converts categorical variables to binary vectors using one-hot encoding

toDummyVarsActors <- function(df, colName) {  
  dfTemp = df
  dfTemp$colName <- strsplit(dfTemp[,colName],"(\\s)?,(\\s)?")  # separate by commas
  unnested <- unnest(dfTemp) # convert to long format with each feature separated with correct corresponding Profit.  
  unnested$colName <- gsub("\\s","_",unnested$colName)  # replace spaces with _
  unnested$colName = chartr("αισ-", "aeo_",unnested$colName)
  unnested$colName <- gsub('"',"",unnested$colName , fixed="TRUE")  
  unnested$colName <- gsub("(","",unnested$colName, fixed=TRUE)  
  unnested$colName <- gsub(")","",unnested$colName, fixed=TRUE)  
  
  unnested$colName <- collapseCat(unnested$colName, colName, threshold = .0015) # remove categories which are of proportion < .0001 for feature selection
  
  dfTemp <- dcast(unnested, Title + Profit ~ `colName`, fun.aggregate = length, value.var = "colName")  # convert to binary vectors with one-hot encoding.  
  return(dfTemp)
}


toDummyVarsWriters <- function(df, colName) {  
  dfTemp = df
  dfTemp$colName <- strsplit(dfTemp[,colName],"(\\s)?,(\\s)?")  # separate by commas
  unnested <- unnest(dfTemp) # convert to long format with each feature separated with correct corresponding Profit.  
  unnested$colName <- gsub("\\s","_",unnested$colName)  # replace spaces with _
  unnested$colName = chartr("αισ-", "aeo_",unnested$colName)
  unnested$colName <- gsub('"',"",unnested$colName , fixed="TRUE")  
  unnested$colName <- gsub("(","",unnested$colName, fixed=TRUE)  
  unnested$colName <- gsub(")","",unnested$colName, fixed=TRUE)  
  
  unnested$colName <- collapseCat(unnested$colName, colName, threshold = .0007) # remove categories which are of proportion < .0001 for feature selection
  
  dfTemp <- dcast(unnested, Title + Profit ~ `colName`, fun.aggregate = length, value.var = "colName")  # convert to binary vectors with one-hot encoding.  
  return(dfTemp)
}

writerBin = toDummyVarsWriters(moviesForBin, "Writer")
actorsBin = toDummyVarsActors(moviesForBin, "Actors")
genreBin = toDummyVars(moviesForBin, "Genre", .0015)
directorBin = toDummyVars(moviesForBin, "Director", .002)
productionBin = toDummyVars(moviesForBin, "Production", .01)
ratedBin = toDummyVars(moviesForBin, "Rated")
languageBin = toDummyVars(moviesForBin, "Language", .005)
countryBin = toDummyVars(moviesForBin, "Country", .003)

# actorsBinTop30DF1 = select(actorsBin, Robert_De_Niro,owen_wilson,ben_stiller,samuel_l._jackson,adam_sandler,johnny_depp,mark_wahlberg,matt_damon,bruce_willis,jack_black,jason_statham,matthew_mcconaughey,nicolas_cage,cameron_diaz,christian_bale,george_clooney,gerard_butler,robert_downey_jr.,will_ferrell,brad_pitt,charlize_theron,julianne_moore,liam_neeson,nicole_kidman,steve_carell,cate_blanchett,ewan_mcgregor,angelina_jolie_pitt,channing_tatum,russell_crowe)

# join the binary matrices
moviesBin = merge(actorsBin, directorBin, by=c("Title","Profit"))
moviesBin = merge(moviesBin, writerBin, by=c("Title","Profit"))
moviesBin = merge(moviesBin, productionBin, by=c("Title","Profit"))
moviesBin = merge(moviesBin, genreBin, by=c("Title","Profit"))
moviesBin = merge(moviesBin, ratedBin, by=c("Title","Profit"))
moviesBin = merge(moviesBin, languageBin, by=c("Title","Profit"))
moviesBin = merge(moviesBin, countryBin, by=c("Title","Profit"))

# 4.	Use linear regression to predict profit based on all available non-numeric variables (using the transformations in (3). Graph the train and test MSE as a function of the train set size (averaged over 10 random data partitions as described above)?


################# BEGIN Q4 RUN FROM HERE ###############################################  

x = movieDrop
y = as.data.frame(as.numeric(movieOnly$Gross - movieOnly$Budget))
colnames(y)[1] = "Profit" # rename column name
xy = bind_cols(x, y) # merge dataframes

#merge original DF for correct dropped rows from X
xy4 = moviesBin
y4 = xy4["Profit"]

x4 = drop(xy4, "Profit")
xy4 = drop(xy4, "Title")

MSEAvTrainList = c()
MSEAvTestList = c()
MSEAvTrainList2 = c()
MSEAvTestList2 = c()

r = nrow(xy4)

#train-test split variable
trainSeq = trunc(seq(5, 95, length = 19))

for (splitVar in trainSeq) {
  cat("\n\n split: ", splitVar, "%, ", 100 - splitVar, "%\n\n")
  
  MSEtrainList = c()
  MSEtestList = c()
  MSEtrainList2 = c()
  MSEtestList2 = c()
  
  #############ADJUST REPETITIONS TO 10 ################
  repetitions = 10 
  
  N = seq(1, repetitions)
  
  while (repetitions > 0) {
    
    #Randomly divide the rows into two sets of sizes 5% and 95%
    
    rand_perm = sample(r, r)
    first_set_of_indices = rand_perm[1:floor(r * splitVar / 100)]
    second_set_of_indices = rand_perm[(floor(r * splitVar / 100) + 1):r]
    trainX = xy[first_set_of_indices, ]
    testX = x[second_set_of_indices, ]

    trainY = y[first_set_of_indices,]
    testY = y[second_set_of_indices,]
    
    X_train <- xy4[first_set_of_indices,]
    X_test <- x4[-first_set_of_indices,]
    
    Y_train <- y4[first_set_of_indices,]
    Y_test <- y4[-first_set_of_indices,]
    
    #and train on the training set
    dfTrain = as.data.frame(trainX)
    
    dfTrain1 = select(dfTrain, Runtime , awardSum , Metascore , imdbRating , imdbVotes , tomatoMeter , tomatoRating , tomatoFresh , tomatoRotten , tomatoUserMeter , tomatoUserReviews, Budget, Profit)
    
    dfTrain1 = na.omit(dfTrain1)
    
    dfTest1 = select(dfTest, Runtime , awardSum ,  Metascore , imdbRating , imdbVotes , tomatoMeter , tomatoRating , tomatoFresh , tomatoRotten , tomatoUserMeter , tomatoUserReviews, Budget, testY)
    
    dfTest1 = na.omit(dfTest1)
    
    dfTest1 = na.omit(dfTest1)
    # ensure testY has the same number of datapoints as testX
    
    testY = dfTest1$testY
    dfTest1 = drop(dfTest1, 'testY')     # remove testY from dfTest1

    # Orginal Model
    
    M1 = lm(Profit~Runtime + awardSum + Metascore + imdbRating + imdbVotes + tomatoMeter + tomatoRating + tomatoFresh + tomatoRotten + tomatoUserMeter +  tomatoUserReviews + Budget, dfTrain1);
    
    ################################## ADJUST MODEL HERE ###############################
    
    M2 = lm(Profit ~ ., X_train);
    
    #theta<-coef(M1)
    
    #summary(M1)$r.squared
    
    MSEtrain = MSEfun(M1)
    #MSEtrain2 = mean((trainPredict - Y_train)^2)
    
    MSEtrain2 = MSEfun(M2)
    
    MSEtest = mean((predict(M1, dfTest1) - testY)^2)
    MSEtest2 = mean((predict(M2, X_test) - Y_test)^2)
    #MSEtest2 = mean((prediction - Y_test)^2)
    
    MSEtrainList = c(MSEtrainList, MSEtrain)
    MSEtestList = c(MSEtestList, MSEtest)
    
    MSEtrainList2 = c(MSEtrainList2, MSEtrain2)
    MSEtestList2 = c(MSEtestList2, MSEtest2)
    
    repetitions = repetitions - 1
  } # end while loop
  
  MSEdf = data.frame(N = N,
                     trainError = MSEtrainList,
                     testError = MSEtestList,
                     trainError2 = MSEtrainList2,
                     testError2 = MSEtestList2)
  print(colMeans(MSEdf))
  #Repeat the random partition of 70% and 30% 10 times and average the test accuracy results over the 10 repetitions.
  
  MSEAvTrainList = c(MSEAvTrainList, mean(MSEdf$trainError))
  MSEAvTestList = c(MSEAvTestList, mean(MSEdf$testError))
  MSEAvTrainList2 = c(MSEAvTrainList2, mean(MSEdf$trainError2))
  MSEAvTestList2 = c(MSEAvTestList2, mean(MSEdf$testError2))
  
} # end for loop

MSEAvDF = data.frame(trainSetSize = trunc(trainSeq * r/100),
                     trainMSE = MSEAvTrainList,
                     testMSE = MSEAvTestList,
                     trainMSE2 = MSEAvTrainList2,
                     testMSE2 = MSEAvTestList2)
MSEAvDF


ggplot(MSEAvDF, aes(trainSetSize)) +
  geom_line(aes(y = trainMSE, color = "OldTrainMSE")) +
  geom_line(aes(y = testMSE, color = "OldTestMSE")) +
  geom_line(aes(y = trainMSE2, color = "NewTrainMSE")) +
  geom_line(aes(y = testMSE2, color = "NewTestMSE")) +
  xlab("Train Set Size") +
  ylab("MSE") +
  ggtitle("Q4 Average Training and Testing MSE vs. Train Set Size")   +  coord_cartesian(ylim = c(3e+16,4e+15))


####################################################################################

#5.	Try to improve the prediction quality in (1) as much as possible by using both numeric and nonnumeric variables as well as creating additional transformed features including interaction features (for example is_genre_comedy x is_budget_greater_than_3M). Explain which transformations you used and why you chose them. Graph the train and test MSE as a function of the train set size (averaged over 10 random data partitions as described above)?

##########Q5##################

##### Add ReleasedMonth as a categorical variable ###########
### From Project1:
# #create new dataframe to separate Released to
ReleasedSep=data.frame(movieDrop$Released)
# separate runtime column into minutes and min
ReleasedSep = separate(ReleasedSep, "movieDrop.Released", c("releasedYear", "releasedMonth", "releasedDay"), "-")

#convert to numeric
# ReleasedSep$ReleasedYear = as.numeric(ReleasedSep$ReleasedYear)
# ReleasedSep$ReleasedMonth = as.numeric(ReleasedSep$ReleasedMonth)
# ReleasedSep$ReleasedDay = as.numeric(ReleasedSep$ReleasedDay)

# add back to dataframe:
movieDrop1 = dplyr::bind_cols(movieDrop, ReleasedSep)

dropList5 = c("Released", "Rated", "Genre", "Director",          "Writer",            "Actors", "Language",          "Country", "tomatoImage", "Production", "Awards", "releasedDay","releasedYear", "awardCategory")

movieDrop5 = drop(movieDrop1, dropList5)

# convert ReleasedMonth into binary variables
realeasedMonthDF = select(movieDrop5, Title, releasedMonth)

releasedMonthBin <- dcast((realeasedMonthDF), Title ~ releasedMonth, fun.aggregate = length, value.var = "releasedMonth")  # convert to binary vectors with one-hot encoding.  

#rename headers
releasedMonthBin = plyr::rename(releasedMonthBin, c("01" = "Month01" , "02" ="Month02",   "03" ="Month03",  "04"  ="Month04",  "05" = "Month05",   "06"="Month06" ,    "07" ="Month07",   "08" ="Month08",   "09"="Month09",    "10"="Month10" ,    "11" = "Month11" ,   "12"= "Month12" ))
                                   
# merge to moviesBin
moviesBin1 = merge(moviesBin, releasedMonthBin, by = c("Title"))

#merge original DF for correct dropped rows from X
x5 = merge(movieDrop5, moviesBin1, by = c("Title"))
x5 = na.omit(x5)
x5 = merge(x5, x[c("Title", "awardCategory")], by = "Title")
y5 = x5["Profit"]

xy5 = x5
x5 = drop(x5, "Profit") # remove profit from x5
# drop title from xy5 and x5
x5 = drop(x5, "Title")
xy5 = drop(xy5, "Title")

# I like to register a multicore backend to parallelize my training
library(glmnet)
## to parallelize
library(doMC)
## to detect the number of cores on your machine
library(parallel)
ncores <- detectCores()
registerDoMC(cores=ncores)


############ Q5 BEGIN RUN HERE #################

r = nrow(x5)

MSEAvTrainList = c()
MSEAvTestList = c()
MSEAvTrainList2 = c()
MSEAvTestList2 = c()

#train-test split variable

trainSeq = trunc(seq(5, 95, length = 19))
for (splitVar in trainSeq) {
  cat("\n\n split: ", splitVar, "%, ", 100 - splitVar, "%\n\n")
  
  MSEtrainList = c()
  MSEtestList = c()
  MSEtrainList2 = c()
  MSEtestList2 = c()
  
  #############ADJUST REPETITIONS TO 10 ################
  repetitions = 50 
  
  N = seq(1, repetitions)
  
  while (repetitions > 0) {
    
    #Randomly divide the rows into two sets of sizes 5% and 95%
    rand_perm = sample(r, r)
    first_set_of_indices = rand_perm[1:floor(r * splitVar / 100)]
    second_set_of_indices = rand_perm[(floor(r * splitVar / 100) + 1):r]
    
    trainX5 = xy5[first_set_of_indices, ]
    testX5 = x5[second_set_of_indices, ]
    
    #and train on the training set
    dfTrain5 = as.data.frame(trainX5)
    #colnames(dfTrain)[length(dfTrain)] = "Profit"
    dfTest5 = as.data.frame(testX5)

    trainY5 = y5[first_set_of_indices,]
    testY5 = y5[second_set_of_indices,]
    
    # Orginal Model
    
    dfTrain1 = select(dfTrain5, Runtime , awardSum , Metascore , imdbRating , imdbVotes , tomatoMeter , tomatoRating , tomatoFresh , tomatoRotten , tomatoUserMeter , tomatoUserReviews, Budget, Profit)
    
    dfTest1 = select(dfTest5, Runtime , awardSum , Metascore , imdbRating , imdbVotes , tomatoMeter , tomatoRating , tomatoFresh , tomatoRotten , tomatoUserMeter , tomatoUserReviews, Budget)
    
    M1 = lm(Profit~Runtime + awardSum + Metascore + imdbRating + imdbVotes + tomatoMeter + tomatoRating + tomatoFresh + tomatoRotten + tomatoUserMeter +  tomatoUserReviews + Budget, dfTrain1);
    
    ################################## ADJUST MODEL HERE #####################################

    #M2 from Q2 plus binary columns added
    # M2 = lm(Profit~Runtime + awardSum * Metascore * imdbRating * imdbVotes + tomatoMeter + tomatoRating + tomatoFresh + tomatoRotten + tomatoUserMeter + Budget + I(awardSum^2) + I(awardSum^3) + I(imdbVotes^2) + I(imdbVotes^3) +  I(tomatoFresh^2) + I(tomatoFresh^3) + I(imdbRating^2) + I(imdbRating^3) + I(Comedy*3) + I(Comedy*4)+ I(Comedy*5) + I(Comedy*11) + I(Comedy*12) + I(Drama*11) + I(Action*5) + I(Adventure*5) + I(Documentary*5)  + I(Crime*11) + I(Thriller*6) + I(Thriller*10) + I(Thriller*11) + I(Romance*11) + I(Animation*5) + I(Animation*6) + . , dfTrain5)
    
    # # only use 1 month per interaction
    # M2 = lm(Profit~Runtime + awardSum * Metascore * imdbRating * imdbVotes + tomatoMeter + tomatoRating + tomatoFresh + tomatoRotten + tomatoUserMeter + Budget + I(awardSum^2) + I(awardSum^3) + I(imdbVotes^2) + I(imdbVotes^3) +  I(tomatoFresh^2) + I(tomatoFresh^3) + I(imdbRating^2) + I(imdbRating^3) + I(Comedy*Month03)  + I(Drama*Month11) + I(Action*Month05) + I(Adventure*Month05) + I(Documentary*Month05)  + I(Crime*Month11) + I(Thriller*Month06) + I(Romance*Month11) + I(Animation*Month05)  + . , dfTrain5)
    
    M2 = lm(Profit~Runtime + awardSum * Metascore * imdbRating * imdbVotes + tomatoMeter + tomatoRating + tomatoFresh + tomatoRotten + tomatoUserMeter + Budget + I(awardSum^2) + I(awardSum^3) + I(imdbVotes^2) + I(imdbVotes^3) +  I(tomatoFresh^2) + I(tomatoFresh^3) + I(imdbRating^2) + I(imdbRating^3) + I(Comedy*Month03) + I(Comedy*Month04) + I(Comedy*Month05) + I(Comedy*Month11) + I(Comedy*Month12) + I(Drama*Month11) + I(Action*Month05) + I(Adventure*Month05) + I(Documentary*Month05)  + I(Crime*Month11) + I(Thriller*Month06) + I(Thriller*Month10) + I(Thriller*Month11)+ I(Romance*Month11) + I(Animation*Month05)  + I(Animation*Month06) + . , dfTrain5)
    
   #coef(M2)
    
    
    MSEtrain = MSEfun(M1)
    MSEtrain2 = MSEfun(M2)
    #MSEtrain2 = mean((trainPredict - trainY5)^2) # using Lasso
    
    MSEtest = mean((predict(M1, dfTest1) - testY5)^2)
    MSEtest2 = mean((predict(M2, dfTest5) - testY5)^2)
    #MSEtest2 = mean((prediction - testY5)^2)  # using Lasso
    
    MSEtrainList = c(MSEtrainList, MSEtrain)
    MSEtestList = c(MSEtestList, MSEtest)
    
    MSEtrainList2 = c(MSEtrainList2, MSEtrain2)
    MSEtestList2 = c(MSEtestList2, MSEtest2)
    
    repetitions = repetitions - 1
  } # end while loop
  
  MSEdf = data.frame(N = N,
                     trainError = MSEtrainList,
                     testError = MSEtestList,
                     trainError2 = MSEtrainList2,
                     testError2 = MSEtestList2)
  print(colMeans(MSEdf))
  #Repeat the random partition of 70% and 30% 10 times and average the test accuracy results over the 10 repetitions.
  
  MSEAvTrainList = c(MSEAvTrainList, mean(MSEdf$trainError))
  MSEAvTestList = c(MSEAvTestList, mean(MSEdf$testError))
  MSEAvTrainList2 = c(MSEAvTrainList2, mean(MSEdf$trainError2))
  MSEAvTestList2 = c(MSEAvTestList2, mean(MSEdf$testError2))
  
} # end for loop

MSEAvDF = data.frame(trainSetSize = trunc(trainSeq * r/100),
                     trainMSE = MSEAvTrainList,
                     testMSE = MSEAvTestList,
                     trainMSE2 = MSEAvTrainList2,
                     testMSE2 = MSEAvTestList2)
MSEAvDF


ggplot(MSEAvDF, aes(trainSetSize)) +
  geom_line(aes(y = trainMSE, color = "OldTrainMSE")) +
  geom_line(aes(y = testMSE, color = "OldTestMSE")) +
  geom_line(aes(y = trainMSE2, color = "NewTrainMSE")) +
  geom_line(aes(y = testMSE2, color = "NewTestMSE")) +
  xlab("Train Set Size") +
  ylab("MSE") +
  ggtitle("Q5 Average Training and Testing MSE vs. Train Set Size")    +  coord_cartesian(ylim = c(2e+16,3e+15))