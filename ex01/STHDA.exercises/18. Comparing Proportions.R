#--------------------------------------------------------#
# 1. Comparing Proportions
#--------------------------------------------------------#
# (1) Compute One-proportion Z-test
#--------------------------------------------------------#
# Target: Compare an observed proportion to a theoretical one.
#--------------------------------------------------------#


# For example, we have a population of mice containing half male and have female (p = 0.5 = 50%).
# Some of these mice (n = 160) have developed a spontaneous cancer, including 95 male and 65 female.



#--------------------------------------------------------#
# 1. By using binom.test{stats} : Exact Binomial Test
#--------------------------------------------------------#
# Recommended when sample size is small
#--------------------------------------------------------#

?binom.test

( fit.binomtest <- binom.test(x = 95, n = 160, p = .5) )

( fit.binomtest <- binom.test(x = 95, n = 160, p = .5, alternative = 'two.sided') )
( fit.binomtest <- binom.test(x = 95, n = 160, p = .5, alternative = 'less') )
( fit.binomtest <- binom.test(x = 95, n = 160, p = .5, alternative = 'greater') )

( fit.binomtest <- binom.test(x = 95, n = 160, p = .5, conf.level = .95) )
( fit.binomtest <- binom.test(x = 95, n = 160, p = .5, conf.level = .99) )

class(fit.binomtest)
mode(fit.binomtest)
names(fit.binomtest)

fit.binomtest$statistic
fit.binomtest$parameter
fit.binomtest$p.value
fit.binomtest$conf.int
fit.binomtest$estimate
fit.binomtest$null.value
fit.binomtest$alternative
fit.binomtest$method
fit.binomtest$data.name



#--------------------------------------------------------#
# 2. By using prop.test{stats} : Test of Equal or Given Proportions
#--------------------------------------------------------#
# when sample size is large ( N > 30). It uses a normal approximation to binomial
#--------------------------------------------------------#

?prop.test

( fit.proptest <- prop.test(x = 95, n = 160, p = .5) )

( fit.proptest <- prop.test(x = 95, n = 160, p = .5, correct = T) )
( fit.proptest <- prop.test(x = 95, n = 160, p = .5, correct = F) )

( fit.proptest <- prop.test(x = 95, n = 160, p = .5, conf.level = .95) )
( fit.proptest <- prop.test(x = 95, n = 160, p = .5, conf.level = .99) )

( fit.proptest <- prop.test(x = 95, n = 160, p = .5, alternative = 'two.sided') )
( fit.proptest <- prop.test(x = 95, n = 160, p = .5, alternative = 'less') )
( fit.proptest <- prop.test(x = 95, n = 160, p = .5, alternative = 'greater') )

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


