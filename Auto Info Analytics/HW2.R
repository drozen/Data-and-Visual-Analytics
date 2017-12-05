#HW2 
#Author: Daniel Rozen, drozen3

library(ggplot2)

# 1. Using the mpg data, describe the relationship between highway mpg and
# car manufacturer. Describe which companies produce the most and least
# fuel efficient cars, and display a graph supporting your conclusion.

data(mpg)

# plot multiple box plots sorted by median hwy mpg vs. manufacturer.  
ggplot(mpg, aes(reorder(manufacturer, -hwy, median), hwy)) +
  geom_boxplot() + # create boxplot
  coord_flip() + # make boxplots sideways
  scale_x_discrete("manufacturer")

# 2. Using the mpg data, explore the three-way relationship between highway
# mpg, city mpg, and model class. What are your observations? Display a
# graph supporting these observations.

# Plot a scatter plot with markers of different colors for class
qplot(x = hwy,
      y = cty,
      data = mpg,
      color = class,
      size=I(4),
      main = "CTY MPG vs. HWY MPG by class")

#multiple panel plot with smoothed line
qplot(x = hwy,
      y = cty,
      facets = class~.,
      data = mpg,
      main = "CTY MPG vs. HWY MPG by class") +
  stat_smooth(se = FALSE)

# 3. What are the pros and cons of using a histogram vs a box plot? Which one
# will you prefer for what purpose?


# 4. Generate two sets of N random points using the function runif and display
# a corresponding scatter plot. 
N = 1000

x = runif(N)
y = runif(N)
DF = data.frame(x = x, y = y) # create dataframe

#qplot(x=x, y=y, data=DF)
ggplot(DF, aes(x = x, y = y)) + geom_point() # generate scatter plot

# plot(x,
#      y,
#      pch = 20,
#      xlab = "x",
#      ylab = "y",
#      main = "Scatter plot of 2 sets of N Random Points")

# ggsave(file="myPlot.jpeg")

# If you save the file to disk, what is the resulting file size for the following 
#file formats: ps, pdf, jpeg, png? 

# dev.off()
# file.size("myPlot.jpeg") # read file size


# How do these values scale with increasing N?

library(ggplot2) 

psSizes = c()
pdfSizes = c()
jpegSizes = c()
pngSizes = c()
# loop for different N sizes
sizes= c(1,10,50,100,500,1000,2500,5000,7500,10000, 15000, 25000, 35000,50000)

for (N in sizes) {
  x = runif(N)
  y = runif(N)
  DF = data.frame(x = x, y = y) # create dataframe
  myplot = ggplot(DF, aes(x = x, y = y)) + geom_point() # generate scatter plot
 # save files
  ggsave(file="myPlot.ps")
  ggsave(file="myPlot.pdf")
  ggsave(file="myPlot.jpeg")
  ggsave(file="myPlot.png")
  
  psSizes = c(psSizes, file.size("myPlot.ps")) # record file sizes
  pdfSizes = c(pdfSizes, file.size("myPlot.pdf")) # save file size
  jpegSizes = c(jpegSizes, file.size("myPlot.jpeg")) # save file size
  pngSizes = c(pngSizes, file.size("myPlot.png")) # save file size
  
}


#create chart

#print dataframe 

df=data.frame(N=sizes, psSizes=psSizes, pdfSizes=pdfSizes, jpegSizes=jpegSizes, pngSizes=pngSizes)
df

# plot filesize as a function of N

ggplot(df, aes(N)) + 
  geom_line(aes(y = psSizes, color = "psSizes")) + 
  geom_line(aes(y = pdfSizes, color = "pdfSizes")) + 
  geom_line(aes(y = jpegSizes, color = "jpegSizes")) + 
  geom_line(aes(y = pngSizes, color = "pngSizes")) + 
  xlab("N") +
  ylab("filesize (bytes)") +
  ggtitle("filesize as a function of N")


# 5. The diamonds dataset within ggplot2 contains 10 columns (price, carat,
#                                                             cut, color, etc.) for 53940 different diamonds. Type help(diamonds) for
# more information. Plot histograms for color, carat, and price, and comment
# on their shapes. Investigate the three-way relationship between price, carat,
# and cut. What are your conclusions? Provide graphs that support your
# conclusions. If you encounter computational difficulties, consider using a
# smaller dataframe whose rows are sampled from the original diamonds
# dataframe. Use the function sample to create a subset of indices that
# may be used to create the smaller dataframe.
library(ggplot2) 

data(diamonds)

# Plot histograms for color, carat, and price

#use bar graph for color since it's discrete

qplot(data=diamonds,color)
#histograms
ggplot(diamonds ,aes(x = carat)) +
  geom_histogram(binwidth = .1)

ggplot(diamonds ,aes(x = price)) +
  geom_histogram(binwidth = 500)


#Investigate the three-way relationship between price, carat,
# and cut.

# Plot a scatter plot with markers of different colors for cut
qplot(x = carat,
      y = price,
      data = diamonds,
      color = cut,
      size=I(1),
      main = "Price vs. carat by cut")

#multiple panel plot with smoothed line
qplot(x = carat,
      y = price,
      facets = cut~.,
      data = diamonds,
      main = "Price vs. carat by cut") +
  stat_smooth(se = FALSE)