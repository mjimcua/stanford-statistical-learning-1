# Question 3

# My answer to get bootstrap estimate of standard error for 
# the Xy data in 5.R.RData  

library(boot)
load("5.R.RData", verbose=T)

bs = function(data, index){
  summary(lm(y~., data=data[index, ]))$coefficients[2, 1]
}

boot(Xy, statistic=bs, R=10000)



# Question 4


# My answer to get bootstrap estimate of B_1's standard error for 
# the Xy data in 5.R.RData. Use blocks of 100 contiguous observations, and 
# resample ten whole blocks with replacement then paste them together to 
# construct each bootstrap time series. For example, one of your bootstrap 
# resamples could be:
# 
# new.rows = c(101:200, 401:500, 101:200, 901:1000, 301:400, 1:100, 1:100, 801:900, 201:300, 701:800)


library(boot)
load("5.R.RData", verbose=T)

bs = function(data, index){
  x <- sample(seq(1, 901, 100), replace=T)
  indicies = as.vector(mapply(seq, from = x, to = x+99))
  summary(lm(y~., data=data[indicies, ]))$coefficients[2, 2]
}

boot(Xy, statistic=bs, R=10000)

