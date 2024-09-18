#--------------------------------------------------------#
# 1. Comparing Proportions
#--------------------------------------------------------#
# (3) Chi-square goodness of fit test
#--------------------------------------------------------#
# Target: Compare multiple observed proportions to expected probabilities.
#--------------------------------------------------------#

# For example, we collected wild tulips and found that 81 were red, 50 were yellow and 27 were white.

# Q1. Are these colors equally common?
#     (If these colors were equally distributed, the expected proportion would be 1/3 for each of the color.)


# Suppose that, in the region where you collected the data, the ratio of red, yellow and white tulip is 3:2:1 (3+2+1 = 6).
# So, 
#   - 3/6 (= 1/2) for red
#   - 2/6 ( = 1/3) for yellow
#   - 1/6 for white

# Q2. We want to know, if there is any significant difference 
#     between the observed proportions and the expected proportions ?


#--------------------------------------------------------#
# chisq.test{stats}: chisq.test(x, p)
#--------------------------------------------------------#
#   x: a numeric vector
#   p: a vector of probabilities of the same length of x



#--------------------------------------------------------#
# 1. Answer to Q1: Are the colors equally common?
#--------------------------------------------------------#
( tulip <- c(80, 50, 27) )

( fit.chisqtest <- chisq.test( tulip, p = c(1/3, 1/3, 1/3) ) )

class(fit.chisqtest)
mode(fit.chisqtest)
names(fit.chisqtest)

fit.chisqtest$statistic
fit.chisqtest$parameter
fit.chisqtest$p.value
fit.chisqtest$method
fit.chisqtest$data.name
fit.chisqtest$observed
fit.chisqtest$expected
fit.chisqtest$residuals
fit.chisqtest$stdres



