#Lesson3 Preprocessing Data
library(ggplot2movies)
names(movies)

mean(movies$length)

mean(movies$budget)

mean(movies$budget, na.rm = TRUE) # take mean with missing values removed

mean(is.na(movies$budget)) # frequency of missing values

# Winsorize data
library(ggplot2)
library(perry)
library(parallel)
library(robustHD)
original_data = c(1000, rnorm(10))
print(original_data)

print(winsorize(original_data))


# Std Mean Quiz: Remove data that is 5SD > / < mean.  Compute sd and mean without outliers
original_data = rnorm(20)
original_data[1] = 1000
sorted_data = sort(original_data)
print(sorted_data)
stdMean = function()
{
  
  #Put the code here 
  # remove outlier for statistical analysis
  outlierRemoved = sorted_data[1:19]
  print(outlierRemoved)
  # compute mean
  mn = mean(outlierRemoved)
  print(mn)
  # compute sd
  stdev = sd(outlierRemoved)
  print(stdev)
  #compute boundary
  bound = 5*stdev
  upper = mn + bound
  lower = mn - bound
  # remove all data that is beyond these boundaries
  data_w_no_outliers = sorted_data[sorted_data<upper & sorted_data>lower]
  #print the original data that has all outliers removed
  print(data_w_no_outliers)
}

stdMean()



# 11.4.1 Random Sampling, Partitioning, and Shuffling
sampled_row_indices = sample(1:4, 3, replace=FALSE)
print(sampled_row_indices)

D = array(data = seq(1, 20, length.out = 20), dim = c(4, 5))
print(D)
D_sampled = D[sampled_row_indices,]
print(D_sampled)

#partition the dataset's rows into two or more
# collection of rows.
D = array(data = seq(1, 20, length.out = 20), dim = c(4, 5))
print(D)
rand_perm = sample(4,4)
rand_perm
first_set_of_indices = rand_perm[1:floor(4*0.75)]
first_set_of_indices
second_set_of_indices = rand_perm[(floor(4*0.75)+1):4]
second_set_of_indices
D1 = D[first_set_of_indices,]
D2 = D[second_set_of_indices,]
print(D1)
D2


# A related task is data Shuffling, which randomly shuffles the dataframe rows.
D = array(data = seq(1, 20, length.out = 20), dim = c(4, 5))
print(D)
D_shuffled = D[sample(4, 4),]
print(D_shuffled)



#RESHAPING DATA:
library(reshape2)
# toy (wide) dataframe in the reshape2 package
smiths
## subject time age weight height
## 1 John Smith 1 33 90 1.87
## 2 Mary Smith 1 NA NA 1.54
# columns 2, 3, 4, 5 are measurements, 1 is key
melt(smiths, id = 1)

# columns 3, 4, 5 are measurements, 1,2 are key
melt(smiths, id = c(1, 2))
## subject time variable value
## 1 John Smith 1 age 33.00

# graph data in facets of sex and time
tips$total.bill = tips$total_bill
qplot(total.bill,
      tip,
      facets = sex~time,
      size = I(1.5),
      data = tips)

# We thus denote the
# tip and total bill as the measurement variables and the remaining variables as
# identifiers.
library(reshape2)
head(tips) # first six rows
tipsm = melt(tips,
             id = c("sex","smoker","day","time","size"))
head(tipsm)
tail(tipsm) # last six rows

# Mean of measurement variables broken by sex.
# Note the role of mean as the aggregating function.
dcast(tipsm,
      sex~variable, # want rows = sex, columns = measurement variable
      fun.aggregate = mean)
# Number of occurrences for measurement variables broken by sex.
# Note the role of length as the aggregating function.
dcast(tipsm,
      sex~variable,
      fun.aggregate = length) #fun.aggregate = length  computes the counts

head(baseball)

# count number of players recorded for each year
bbPerYear = ddply(baseball, "year", "nrow")
head(bbPerYear)

qplot(x = year,
      y = nrow,
      data = bbPerYear,
      geom = "line",
      ylab="number of player seasons")

# compute mean rbi (batting attempt resulting in runs)
# for all years. Summarize is the apply function, which
# takes as argument a function that computes the rbi mean
bbMod=ddply(baseball,
            "year",
            summarise,
            mean.rbi = mean(rbi, na.rm = TRUE))
head(bbMod)

qplot(x = year,
      y = mean.rbi,
      data = bbMod,
      geom = "line",
      ylab = "mean RBI")

# add a column career.year which measures the number of years passed
# since each player started batting
bbMod2 = ddply(baseball,
               "id",
               transform,
               career.year = year - min(year) + 1)
# sample a random subset 3000 rows to avoid over-plotting
bbSubset = bbMod2[sample(dim(bbMod2)[1], 3000),]
qplot(career.year,
      rbi, data = bbSubset,
      size = I(0.8),
      geom = "jitter",
      ylab = "RBI",
      xlab = "years of playing") +
  geom_smooth(color = "red", se = F, size = 1.5)

library(plyr)
head(ozone)
tail(ozone)
View(ozone)

ozone