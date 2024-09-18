#--------------------------------------------------------#
# 1. Comparing Proportions
#--------------------------------------------------------#
# (2) Compute two-proportions z-test
#--------------------------------------------------------#
# Target: Compare two observed proportions.
#--------------------------------------------------------#

# For example, we have two groups of individuals:
#
#   Group A, with lung cancer   : n = 500
#   Group B, healthy individuals: n = 500

# The number of smokers in each group is as follow:
#
#   Group A  with lung cancer   : n = 500, 490 smokers, pA=490/500=98
#   Group B, healthy individuals: n = 500, 400 smokers, pB=400/500=80


# Q: Whether the proportions of smokers are the same in the two groups of individuals



#--------------------------------------------------------#
# 1. By using prop.test{stats} : Test of Equal or Given Proportions
#--------------------------------------------------------#
# Recommended when sample size is large
#--------------------------------------------------------#

?prop.test

( fit.proptest <- prop.test(x = c(490, 400), n = c(500, 500)) )

( fit.proptest <- prop.test(x = c(490, 400), n = c(500, 500), alternative = 'two.sided') )
( fit.proptest <- prop.test(x = c(490, 400), n = c(500, 500), alternative = 'less') )
( fit.proptest <- prop.test(x = c(490, 400), n = c(500, 500), alternative = 'greater') )

( fit.proptest <- prop.test(x = c(490, 400), n = c(500, 500), correct = T) )
( fit.proptest <- prop.test(x = c(490, 400), n = c(500, 500), correct = F) )

( fit.proptest <- prop.test(x = c(490, 400), n = c(500, 500), conf.level = .95) )
( fit.proptest <- prop.test(x = c(490, 400), n = c(500, 500), conf.level = .99) )

class(fit.proptest)
mode(fit.proptest)
names(fit.proptest)

fit.proptest$statistic
fit.proptest$parameter
fit.proptest$p.value
fit.proptest$estimate
fit.proptest$null.value
fit.proptest$conf.int
fit.proptest$alternative
fit.proptest$method
fit.proptest$data.name



#--------------------------------------------------------#
# 2. By using fisher.test{stats} : Fisher's Exact Test for Count Data
#--------------------------------------------------------#
# Recommended when sample size is small
#--------------------------------------------------------#

# The Fisher Exact probability test is an excellent non-parametric technique for comparing proportions, 
# when the two independent samples are small in size.


