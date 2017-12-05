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


