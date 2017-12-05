#HW1
#Author: Daniel Rozen, drozen3

# #2.Implement a function that computes the log of the factorial value of an
# integer using a for loop. Note that implementing it using log(A)+log(B)+
#   · · · avoids overflow while implementing it as log(A · B · · · · ) creates an
# overflow early on.

myFun2 = function(x) {
  if (is.numeric(x)) { # check if integer
  sum=0
  for (i in 1:(x)){
    sum=sum+log(i)
  }
  return(sum)
  } 
}

# 3. Implement a function that computes the log of the factorial value of an
# integer using recursion.

myFun3= function(x) {
  if (is.numeric(x)) { # check if integer
  
    if (x==1) {
      return(log(1))
    }
      
    currentSum = log(x) + myFun3(x-1)
    return(currentSum)
    
  }
}

# 4. Using your two implementations of log-factorial in (2) and (3) above, compute
# the sum of the log-factorials of the integers 1, 2, . . . ,N for various N
# values.   

# using for loop (probably simplest) for the sum in both cases

sumFun2= function(x) {
  if (is.numeric(x)) { # check if integer
    sum=0
    for (i in 1:(x)){
      sum=sum+myFun2(i)
    }
    return(sum)
  } 
}

sumFun3= function(x) {
  if (is.numeric(x)) { # check if integer
    sum=0
    for (i in 1:(x)){
      sum=sum+myFun3(i)
    }
    return(sum)
  } 
}


# 5. Compare the execution times of your two implementations for (4) with an
# implementation based on the official R function lfactorial(n). You
# may use the function system.time() to measure execution time. 


# an implementation based on the official R function lfactorial(n)
sumLFact= function(x) {
  if (is.numeric(x)) { # check if integer
    sum=0
    for (i in 1:(x)){
      sum=sum+lfactorial(i)
    }
    return(sum)
  } 
}

#What
# are the growth rates of the three implementations as N increases? Use the
# command options(expressions=500000) to increase the number of
# nested recursions allowed. Compare the timing of the recursion implementation
# as much as possible, and continue beyond that for the other two
# implementations.

options(expressions=500000)

#create chart

N = seq(100, 3000, length = 15)
time2 = c()
time3 = c()
timeLFact = c()

for (n in N) {
  time2= c(time2, system.time(sumFun2(n))[3]) # use elapsed time
  time3= c(time3, system.time(sumFun3(n))[3]) 
  timeLFact= c(timeLFact, system.time(sumLFact(n))[3]) 
}

#print dataframe 

df=data.frame(N=N, time2=time2, time3=time3, timeLFact=timeLFact)
df

# plot run time as a function of array size for R and
# .C implementations

library(ggplot2) 

ggplot(df, aes(N)) + 
  geom_line(aes(y = time2, color = "sumFun2")) + 
  geom_line(aes(y = time3, color = "sumFun3")) + 
  geom_line(aes(y = timeLFact, color = "timeLFact")) + 
                xlab("N") +
                ylab("Seconds") +
                ggtitle("Seconds to Run")

