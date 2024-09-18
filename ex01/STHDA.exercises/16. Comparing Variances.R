#--------------------------------------------------------#
# 1. Comparing Variances (F-test)
#--------------------------------------------------------#
# F-Test: Compare Two Variances
#--------------------------------------------------------#


#--------------------------------------------------------#
# 1. Import and check your data
#--------------------------------------------------------#

?ToothGrowth

dim(ToothGrowth)
str(ToothGrowth)
head(ToothGrowth)
View(ToothGrowth)

table(ToothGrowth$supp, ToothGrowth$dose)

( df <- ToothGrowth )


dplyr::sample_n(df, 10)


#--------------------------------------------------------#
# 2. Preleminary test to check F-test assumptions
#--------------------------------------------------------#

# 2-1. Visual methods
library(ggpubr)

ggdensity(df$len)
ggdensity(df$len, main = 'Density of tooth length')
ggdensity(df$len, main = 'Density of tooth length', linetype = 'dashed')
ggdensity(df$len, main = 'Density of tooth length', linetype = 'dashed', color = 'blue')
ggdensity(df$len, main = 'Density of tooth length', fill = 'skyblue')
ggdensity(df$len, title = 'Density of tooth length', xlab = 'Tooth length', ylab = 'Density')

# 2-2. Q-Q plot
library(ggpubr)

ggqqplot(df$len)


library(car)

?qqPlot
qqPlot(df$len)

qqPlot(df$len, simulate = FALSE)   # 사용되지 않은 인자 (simulate = FALSE)
qqPlot(df$len, reps = 10)          # 사용되지 않은 인자 (reps = 10)

qqPlot(df$len, id = FALSE)
qqPlot(df$len, id = TRUE)

qqPlot(df$len, lwd = 1)

qqPlot(df$len, lwd = 1, pch = 1)
qqPlot(df$len, lwd = 1, pch = 2)
qqPlot(df$len, lwd = 1, pch = 3)
qqPlot(df$len, lwd = 1, pch = 4)
qqPlot(df$len, lwd = 1, pch = 5)
qqPlot(df$len, lwd = 1, pch = 6)
qqPlot(df$len, lwd = 1, pch = 7)
qqPlot(df$len, lwd = 1, pch = 8)
qqPlot(df$len, lwd = 1, pch = 9)
qqPlot(df$len, lwd = 1, pch = 10)
qqPlot(df$len, lwd = 1, pch = 11)
qqPlot(df$len, lwd = 1, pch = 12)
qqPlot(df$len, lwd = 1, pch = 13)
qqPlot(df$len, lwd = 1, pch = 14)
qqPlot(df$len, lwd = 1, pch = 15)
qqPlot(df$len, lwd = 1, pch = 16)
qqPlot(df$len, lwd = 1, pch = 17)
qqPlot(df$len, lwd = 1, pch = 18)
qqPlot(df$len, lwd = 1, pch = 19)
qqPlot(df$len, lwd = 1, pch = 20)
qqPlot(df$len, lwd = 1, pch = 21)
qqPlot(df$len, lwd = 1, pch = 22)
qqPlot(df$len, lwd = 1, pch = 23)
qqPlot(df$len, lwd = 1, pch = 24)

qqPlot(df$len, lwd = 1, pch = 24, cex = 1)
qqPlot(df$len, lwd = 1, pch = 24, cex = 2)
qqPlot(df$len, lwd = 1, pch = 24, cex = 3)

qqPlot(df$len, lwd = 2, envelope = .95)
qqPlot(df$len, lwd = 2, envelope = .99)

qqPlot(df$len, lwd = 2, envelope = .99, col = 'red')
qqPlot(df$len, lwd = 2, envelope = .99, col = 'red', col.lines = 'black')

qqPlot(df$len, main='Q-Q plot of Tooth length')
qqPlot(df$len, main='Q-Q plot of Tooth length', xlab = 'Theoretical', ylab = 'Length')

qqPlot(df$len, main='Q-Q plot of Tooth length', xlab = 'Theoretical', ylab = 'Length', las = 0)
qqPlot(df$len, main='Q-Q plot of Tooth length', xlab = 'Theoretical', ylab = 'Length', las = 1)


# 2-3. Shapiro Wilk's Test
?shapiro.test

shapiro.test(df$len)  # between 3 and 5000.



#--------------------------------------------------------#
# 3. Compute F-test
#--------------------------------------------------------#

( fit.ftest <- var.test(len ~ supp, data = df) )

class(fit.ftest)
mode(fit.ftest)
names(fit.ftest)

fit.ftest$statistic
fit.ftest$parameter
fit.ftest$p.value
fit.ftest$conf.int
fit.ftest$estimate
fit.ftest$null.value
fit.ftest$alternative
fit.ftest$method
fit.ftest$data.name

