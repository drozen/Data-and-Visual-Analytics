#Project1
# Author: Daniel Rozen, drozen3

library(ggplot2)
library(plyr)
library(tidyr)
library(dplyr)
library(reshape2)

load('movies_merged')
# create copy to modify
movieOnly=data.frame(movies_merged)

# 1. The variable Type captures whether the row is a movie, a TV series, or a game.  
#Remove all rows that do not correspond to movies.
movieOnly= subset(movieOnly, Type == "movie")
# How many rows did you remove?
dim(subset(movieOnly, Type != "movie"))


# 2. The variable Runtime represents the length of the title as a string. 

#Write R code to convert it to a numeric value (in minutes) 


#create new dataframe to copy Runtime to
runTime=data.frame(movieOnly$Runtime)
#create new dataframe to separate Runtime to
runTimeSep=data.frame(movieOnly$Runtime)
# separate runtime column into minutes and min
runTimeSep = separate(runTime, "movieOnly.Runtime", c("hours", "h", "minutes", "min"), " ")

# #remove NA entries
# completeFun <- function(data, desiredCols) {
#   completeVec <- complete.cases(data[, desiredCols])
#   return(data[completeVec, ])
# }

hoursCol = as.numeric(runTimeSep$hours) # extract the hours digits and convert to numeric values
runTimeSep$hours = hoursCol

minutesCol = as.numeric(runTimeSep$minutes) # extract the minute digits and convert to numeric values
runTimeSep$minutes = minutesCol

#convert hours to minutes
runTimeSep$newMins = with(runTimeSep, (h=="h")*60*hours + (h=="min")*hours + ifelse(is.na(minutes), 0, minutes))

movieOnly$Runtime = runTimeSep$newMins #replace runtime with  the new numeric column

newMins = runTimeSep$newMins
runTimeSep$newMins = newMins
#winsorize runTimeSep
library(robustHD)
winsorizedNewMins = winsorize(na.omit(newMins))
winsorizedRuntimeDF = data.frame(winsorizedNewMins)

#Investigate and describe the distribution of that value and comment on how it changes 

#Runtime histogram plot
ggplot(movieOnly ,aes(x = Runtime)) +
  geom_histogram(binwidth = 5) 

# Winsorized Runtime histogram plot
ggplot(winsorizedRuntimeDF ,aes(x = winsorizedNewMins)) +
  geom_histogram(binwidth = 4) 

# scatter plot
plot(movieOnly$Runtime,
     pch = 20, # marker
     cex = .1, # size
     xlab="Sample number",
     ylab = "Runtimes (min)",
     main = "Run Times")

#box plot
ggplot(movieOnly, aes("",Runtime)) +
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete("")

#box plot winsorized
ggplot(winsorizedRuntimeDF, aes("",winsorizedNewMins)) +
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete("")




#Investigate and describe the distribution of that value and comment on how it changes 

#Runtime histogram plot
ggplot(movieOnly ,aes(x = Runtime)) +
  geom_histogram(binwidth = 5) 


# scatter plot
plot(movieOnly$Runtime,
     pch = 20, # marker
     cex = .1, # size
     xlab="Sample number",
     ylab = "Runtimes (min)",
     main = "Run Times")

#box plot
ggplot(movieOnly, aes("",Runtime)) +
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete("")

# compute mean Runtime per year

meanRunTimeYear=ddply(movieOnly,
                      "Year",
                      summarise,
                      mean.Runtime = mean(Runtime, na.rm = TRUE))

meanRunTimeBudget=ddply(movieOnly,
                        "Budget",
                        summarise,
                        mean.Runtime = mean(Runtime, na.rm = TRUE))

# graph mean Runtime vs. Year

qplot(Year,
      mean.Runtime,
      geom="line",
      data=meanRunTimeYear,
      ylab = "mean RunTime (mins)")

# graph Runtime vs. Budget
qplot(Budget,
      mean.Runtime,
      geom="line",
      data=meanRunTimeBudget,
      ylab = "mean RunTime (mins)")



# 3. The column Genre represents a list of genres associated with the movie in a string format. 

# Convert genreVector into a corpus in order to parse each text string into a binary vector with 1s representing the presence of a genre and 0s the absence 
library(tm)

genreVector = movieOnly$Genre
genreVector = gsub(",", "", genreVector) # remove commas

genreCorpus = Corpus(VectorSource(genreVector))
#dtm = DocumentTermMatrix(genreCorpus, list(dictionary=genreNames))
dtm = DocumentTermMatrix(genreCorpus)
binaryGenreVector = inspect(dtm)

#add it to the dataframe as additional columns:

binaryGenreDF = data.frame(binaryGenreVector) # convert binaryGenreVector to dataframe

movieOnlyWithBinary = dplyr::bind_cols(movieOnly, binaryGenreDF) # merge dataframes

#Graph and describe the relative proportions of titles having the top 10 genres  

# use bar graph

# sum up columns to find genre totals
library(slam)
genreCounts = rollup(binaryGenreVector, 1, na.rm=TRUE, FUN = sum)
genreCountsDF = data.frame(genreCounts) #convert to DF
# sort DF to find the top 10
genreCountsSortedDF = (ddply(genreCountsDF, 1, sort, decreasing = TRUE))
#convert to proportions in percentages by dividing by the top 10 total
genreCountsSortedDFProp = prop.table(genreCountsSortedDF)


# only select the top 10 & transpose for bar polots
genreCountsSortedDFPropTop10 = data.frame(t(genreCountsSortedDFProp[, (seq(1,10))]))

# print table of top 10 proportions
genreCountsSortedDFPropTop10

# Bar Plot of top 10 in descending order of proportion
ggplot(data = genreCountsSortedDFPropTop10, 
       mapping = aes(x = reorder(row.names(genreCountsSortedDFPropTop10), -(t.genreCountsSortedDFProp....seq.1..10....)), y=t.genreCountsSortedDFProp....seq.1..10....)) + geom_bar(stat = "identity") + ylab("Proportions") + xlab("Genres") +ggtitle("Relative proportions of titles having the top 10 genres")

ggplot(data = genreCountsSortedDFPropTop10, 
       mapping = aes(x = row.names(genreCountsSortedDFPropTop10), y=t.genreCountsSortedDFProp....seq.1..10....)) + geom_bar(stat = "identity") + ylab("Proportions") + xlab("Genres") +ggtitle("Relative proportions of titles having the top 10 genres")

# examine how the distribution of gross revenue (variable Gross) changes across genres.

# remove Gross NA columns
movieOnlyWithBinaryGross = movieOnlyWithBinary[!is.na(movieOnlyWithBinary$Gross),]

# select only Gross and Genre columns
grossGenreOnly = dplyr::select(movieOnlyWithBinaryGross, Gross, drama, comedy, short, romance, action, crime, thriller, documentary, adventure, animation)

# convert to long format
grossGenreLong = melt(grossGenreOnly,
             id = (row.names(genreCountsSortedDFPropTop10)))

# convert to long format with Gross and genre column under variable
grossOnlyGenreLong = melt(grossGenreOnly,
                          id = "Gross")

#remove negative binary results
grossOnlyGenreLong = grossOnlyGenreLong[(grossOnlyGenreLong$value==1),]

#use a boxplot to view gross revenue distribution across genres

#todo: read data_processing.pdf page 29 and onwards 
# calculate mean budget for each genre alone by isolating dataframes for each genre?

# plot multiple box plots of Gross Distribution vs. mean

ggplot(grossOnlyGenreLong, aes(reorder(variable, -Gross, median), Gross)) +
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete("Genre")

# zoom
ggplot(grossOnlyGenreLong, aes(reorder(variable, -Gross, median), Gross)) +
  geom_boxplot() +
  scale_x_discrete("Genre") +
  coord_flip(ylim = c(10000000,500000000)) 

# convert to log x scale
ggplot(grossOnlyGenreLong, aes(reorder(variable, -Gross, median), Gross)) +
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete("Genre") +
  scale_y_log10()


# 4. Find and remove all rows where you
# suspect a merge error occurred based on a mismatch between these two variables. 

#To make sure subsequent analysis and modeling work well, avoid removing more than 10% of the rows that have a present Gross variable. 

# break up Released into component rows with ReleasedYear and ReleasedMonth-Day

# #create new dataframe to separate Released to
ReleasedSep=data.frame(movieOnlyWithBinaryGross$Released)
# separate runtime column into minutes and min
ReleasedSep = separate(ReleasedSep, "movieOnlyWithBinaryGross.Released", c("ReleasedYear", "ReleasedMonth", "ReleasedDay"), "-")

#convert to numeric
ReleasedSep$ReleasedYear = as.numeric(ReleasedSep$ReleasedYear)
ReleasedSep$ReleasedMonth = as.numeric(ReleasedSep$ReleasedMonth)

ReleasedSep$ReleasedDay = as.numeric(ReleasedSep$ReleasedDay)


# add back to dataframe:
movieOnlyWithBinaryGross = dplyr::bind_cols(movieOnlyWithBinaryGross, ReleasedSep)

# compare Year with Released Year and remove rows with mismatches with Released year greater than 2 years after YEar
discrepency = movieOnlyWithBinaryGross$Year - movieOnlyWithBinaryGross$ReleasedYear


movieOnlyWithBinaryGrossBadDataRemoved = dplyr::filter(movieOnlyWithBinaryGross, (ReleasedYear - Year) <= 2 | is.na(ReleasedYear) == TRUE)

#calculate rows removed
rowsRemoved = nrow(movieOnlyWithBinaryGross) - nrow(movieOnlyWithBinaryGrossBadDataRemoved)
#calculate % rows removed
percentRowsRemoved = 100*rowsRemoved/nrow(movieOnlyWithBinaryGross)
cat("rowsRemoved =", rowsRemoved)
cat("percentRowsRemoved =", percentRowsRemoved)


# 5. An important question is when to release a movie. 

# Investigate the relationship between release date and gross revenue and comment on what times of year are most high revenue movies released in. 

# investigate mean gross revenue vs. month

# compute mean gross revenue per month

meanGrossMonth = ddply(movieOnlyWithBinaryGrossBadDataRemoved,
                      "ReleasedMonth",
                      summarise,
                      mean.Gross = mean(Gross, na.rm = TRUE))
meanGrossMonth
# graph bar plot  mean gross revenue vs. month released

#barplot(meanGrossMonth, main="relative proportions of titles having the top 10 genres" )

ggplot(data = meanGrossMonth, 
       mapping = aes(x = factor(ReleasedMonth), y=mean.Gross)) + geom_bar(stat = "identity") + ylab("mean Gross ($)") + xlab("Month") +ggtitle("mean gross revenue vs. month released")

#Does your answer changes for different genres? 

# plot bar graphs on facets:

#create dataframe with month data added with gross:

# select only Gross and top 10 Genre columns
grossGenreMonth = dplyr::select(movieOnlyWithBinaryGrossBadDataRemoved, Gross, ReleasedMonth, drama, comedy, short, romance, action, crime, thriller, documentary, adventure, animation)

# convert to long format with Gross and genre column under variable
grossGenreMonthLong = melt(grossGenreMonth,
                          id = c("Gross", "ReleasedMonth"))

#remove negative binary results
grossGenreMonthLong = grossGenreMonthLong[(grossGenreMonthLong$value==1),]


grossGenreMonthLongDrama = filter(grossGenreMonthLong, variable =='drama')

meanGrossGenreMonth = ddply(grossGenreMonthLong,
                       .(ReleasedMonth, variable),
                       summarise,
                       mean.Gross = mean(Gross, na.rm = TRUE))

# count number of movies by Genre
genreCount = ddply(grossGenreMonthLong, "variable", "nrow")
genreCount

# plot bar plots with facets
ggplot(meanGrossGenreMonth, aes(x = factor(ReleasedMonth), y=mean.Gross)) + geom_bar(stat = "identity") + facet_wrap(~ variable, scales="free_y") +ylab("mean Gross ($)") + xlab("Month") +ggtitle("Mean gross revenue vs. month released by Genre")


# 6. There are several variables that describe ratings including IMDb ratings (imdbRating
#                                                                              represents average user ratings and imdbVotes represents the number of user ratings) and
# multiple Rotten Tomatoes ratings (represented by several variables pre-fixed by tomato). Read
# up on such ratings on the web (for example rottentomatoes.com/about and http://
#                                  www.imdb.com/help/show_leaf?votestopfaq) and 

#investigate the pairwise relationships between these different descriptors using graphs. 
library(GGally)
# select columns to use with GG pairs

ggpairsDF = select(movieOnly, imdbRating, tomatoMeter, tomatoRating, tomatoUserMeter, tomatoUserRating) 
# rename columns for easier graph readability
ggpairsDF = plyr::rename(ggpairsDF, c("imdbRating" = "imdbRat", "tomatoMeter"="tomatoMet", "tomatoRating" = "tomatoRat", "tomatoUserMeter"= "tomUserMet", "tomatoUserRating" = "tomUseRat" ))
names(ggpairsDF)

ggcorr(ggpairsDF, method="na.or.complete")
ggpairs(ggpairsDF)

#Comment on similarities and differences
# between the user ratings of IMDb and the critics ratings of Rotten Tomatoes.

# focus on user ratings of IMDb and the critics ratings of Rotten Tomatoes.

ggpairsDFFocused = select(movieOnly, imdbRating, tomatoMeter, tomatoRating) 
# rename columns for easier graph readability
ggpairsDFFocused = plyr::rename(ggpairsDFFocused, c("imdbRating" = "imdbRat", "tomatoMeter"="tomatoMet", "tomatoRating" = "tomatoRat" ))
names(ggpairsDFFocused)
# graph correlation charts
ggcorr(ggpairsDFFocused, method="na.or.complete")
ggpairs(ggpairsDFFocused)

#Comment on similarities and differences between the user ratings of IMDb and the critics ratings of Rotten Tomatoes. 

#  Comment on the relationships between these variables and the gross revenue.

# select ratings variables

grossPairsDfRatings = ggpairsDF = select(movieOnlyWithBinaryGrossBadDataRemoved, Gross, imdbRating, tomatoMeter, tomatoRating, tomatoUserMeter, tomatoUserRating)

grossPairsDfRatings = plyr::rename(grossPairsDfRatings, c("Gross" = "Gross", "imdbRating" = "imdbRat", "tomatoMeter"="tomatoMet", "tomatoRating" = "tomatoRat", "tomatoUserMeter"= "tomUserMet", "tomatoUserRating" = "tomUseRat"))

# graph correlation charts using ggpairs and cor()
ggcorr(grossPairsDfRatings, method="na.or.complete")
ggpairs(grossPairsDfRatings,
        upper = list(continuous = wrap("cor", use = "na.or.complete")))

# select non-ratings variables

grossPairsDf = ggpairsDF = select(movieOnlyWithBinaryGrossBadDataRemoved, Gross, imdbVotes, tomatoReviews, tomatoFresh, tomatoRotten, tomatoUserReviews)

grossPairsDf = plyr::rename(grossPairsDf, c("Gross" = "Gross", "imdbVotes" = "iVot", "tomatoReviews"="tRev", "tomatoFresh" = "tFres", "tomatoRotten"= "tRot", "tomatoUserReviews" = "tURev"))

# graph correlation charts
ggcorr(grossPairsDf, method="na.or.complete")

ggpairs(grossPairsDf,
        upper = list(continuous = wrap("cor", use = "na.or.complete")))

# 7.  Convert Awards to a threedimensional binary vector whose:

# 1 component represents no nomination or awards, (this includes NA values)

# 2 component represents some nominationsawards, 

#3 component represents many nominations or awards. 

#create new dataframe to copy awardsDF to
library(stringr)

# extract numbers
  inputX = movieOnlyWithBinaryGrossBadDataRemoved$Awards
  head(inputX)
  x = (regmatches(inputX, gregexpr('\\(?[0-9]+', inputX)))
  head(x)
  x = lapply(x, as.numeric) # convert to numeric
  xsum = lapply(x, sum) # sum up each row
  head(xsum)
  # TODO: create function that converts each row to binary vector 
  # output quantiles for upper range determination for the following classification function
  quantile(unlist(xsum),seq(0, 1, length.out = 41))
  
  convertFun = function(x) {
    # upperThreshold = meanAwards + sdAwards*.5
    upperThreshold = 37
    if (x == 0) {x = "none"} 
  else if (x >=1 & x<=upperThreshold) {x="some"}
  else if (x > upperThreshold) {x= "many"}
  }
  categorized = lapply(xsum, convertFun)

  categorizedDF = data.frame(unlist(xsum), unlist(categorized))
  # rename columns
  categorizedDF = plyr::rename(categorizedDF, c("unlist.xsum." = "awardSum", "unlist.categorized."="awardCategory"))

  # Convert categorizedDF into a corpus in order to parse each text string into a binary vector with 1s representing each category
  
  library(tm)
  awardVector = categorizedDF$awardCategory
  awardCorpus = Corpus(VectorSource(awardVector))
  awardDtm = DocumentTermMatrix(awardCorpus)
  binaryAwardVector = inspect(awardDtm)
  
  #add it to the dataframe as additional columns:
  
  binaryAwardDF = data.frame(binaryAwardVector) # convert binaryawardVector to dataframe
  
  movieOnlyWithBinaryGrossBadDataRemoved = dplyr::bind_cols(movieOnlyWithBinaryGrossBadDataRemoved, binaryAwardDF) # merge dataframes
  
  movieOnlyWithBinaryGrossBadDataRemoved = dplyr::bind_cols(movieOnlyWithBinaryGrossBadDataRemoved, categorizedDF) # merge dataframes
  
  # sum up columns to find award totals
  library(slam)
  AwardCounts = rollup(binaryAwardVector, 1, na.rm=TRUE, FUN = sum)
  AwardCountsDF = data.frame(AwardCounts) #convert to DF
 
  AwardCountsDF
  ratio = AwardCountsDF$some/AwardCountsDF$many
  print(ratio)
  
  # How does the gross revenue distribution changes across these three categories.
  
  # plot multiple box plots of Gross Distribution vs. mean
  
  ggplot(movieOnlyWithBinaryGrossBadDataRemoved, aes(reorder(awardCategory, -Gross, median), Gross)) +
    geom_boxplot() +
    coord_flip() +
    scale_x_discrete("Category")
  
  #zoom in
  
  ggplot(movieOnlyWithBinaryGrossBadDataRemoved, aes(reorder(awardCategory, -Gross, median), Gross)) +
    geom_boxplot() +
    coord_flip(ylim = c(0,1000000000)) +
    scale_x_discrete("Category")
  
  # convert to log x scale
  ggplot(movieOnlyWithBinaryGrossBadDataRemoved, aes(reorder(awardCategory, -Gross, median), Gross)) +
    geom_boxplot() +
    coord_flip() +
    scale_x_discrete("Category")
    scale_y_log10()

# #winsorize runTimeSep
# winsorizedNewMins = winsorize(na.omit(newMins))
# winsorizedRuntimeDF = data.frame(winsorizedNewMins)

# 8. Come up with two new insights (backed up by the data and graphs) that are expected, and one
# new insight (backed up by data and graphs) that is unexpected at first glance and do your best
# to motivate it. By "new" here I mean insights that are not an immediate consequence of one of
# the above assignments.

    # calculate correlation between Gross and awardSum 
    ggpairsDFFocused1 = select(movieOnlyWithBinaryGrossBadDataRemoved, Gross, awardSum) 
    
    ggpairs(ggpairsDFFocused1)    
    
    # calculate correlation between Gross and Year 
    ggpairsDFFocused1 = select(movieOnlyWithBinaryGrossBadDataRemoved, Gross, Year) 
    
    ggpairs(ggpairsDFFocused1)    
    
    # mean gross revenue vs. year
    meanGrossPerYear = ddply(
      movieOnlyWithBinaryGrossBadDataRemoved,
      "Year",
      summarise,
      mean.gross = mean(Gross, na.rm = TRUE),
      mean.awardSum = mean(awardSum, na.rm = TRUE)
    )
    head(meanGrossPerYear)

    qplot(x=Year,
          y= mean.gross,
          data=meanGrossPerYear,
          geom="line",
          ylab = "mean Gross Revenue ($)" ) +
            #geom_smooth(color="red", size= 1) +
    stat_smooth(color="red", size=I(2), se=F)
    
    qplot(x=Year,
         y= mean.awardSum,
         data=meanGrossPerYear,
         geom="line",
         ylab = "mean.awardSum" ) +
      #geom_smooth(color="red", size= 1) +
      stat_smooth(color="red", size=I(2), se=F)
    
    # total awards per year
    totalAwardsYear = ddply(movieOnlyWithBinaryGrossBadDataRemoved,
                            "Year",
                            summarise,
                            tot.awardSum = sum(awardSum),
                            #mean.awardSum = mean(awardSum, na.rm = TRUE)
    )
    head(totalAwardsYear)

# Calculate Mean Gross Vs Genre DF
MeanGrossVsGenreDF = data.frame(list(Genre = row.names(genreCountsSortedDFPropTop10), Gross= c(drama[2,2], comedy[2,2], short[2,2], romance[2,2], action[2,2], crime[2,2], thriller[2,2], documentary[2,2], adventure[2,2], animation[2,2])))

# Descending Bar Graph of Mean Gross Revenues for the top 10 genres

ggplot(data = MeanGrossVsGenreDF, 
       mapping = aes(x = reorder(MeanGrossVsGenreDF$Genre, -MeanGrossVsGenreDF$Gross), y=MeanGrossVsGenreDF$Gross)) + geom_bar(stat = "identity") + ylab("Mean Gross Revenue ($)") + xlab("Genres") +ggtitle("Mean Gross Revenues for the top 10 genres")

plot(movieOnlyWithBinaryGrossBadDataRemoved$Year,
     movieOnlyWithBinaryGrossBadDataRemoved$tomatoReviews,
     xlab = "Year",
     ylab = "tomatoReviews")

# mean TomatoeReviews vs. year
meanTomRevPerYear = ddply(
  movieOnlyWithBinaryGrossBadDataRemoved,
  "Year",
  summarise,
  mean.tomatoReviews = mean(tomatoReviews, na.rm = TRUE)
)
head(meanTomRevPerYear)

qplot(x=Year,
      y= mean.tomatoReviews,
      data=meanTomRevPerYear,
      geom="line",
      ylab = "mean.tomatoReviews" ) +
  #geom_smooth(color="red", size= 1) +
  stat_smooth(color="red", size=I(2), se=F)

