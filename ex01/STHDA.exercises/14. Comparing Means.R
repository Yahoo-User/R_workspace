#--------------------------------------------------------#
# 1. Comparing the means of more than two groups
#--------------------------------------------------------#
# Analysis of variance (ANOVA, parametric)
#--------------------------------------------------------#
#   (2) Two-Way ANOVA Test for unbalanced designs
#--------------------------------------------------------#


#--------------------------------------------------------#
# 1. Import your data into R
#--------------------------------------------------------#

?ToothGrowth

dim(ToothGrowth)
head(ToothGrowth)
str(ToothGrowth)
View(ToothGrowth)

table(ToothGrowth$supp)

df <- ToothGrowth
df



#--------------------------------------------------------#
# 2. Compute two-way ANOVA test for unbalanced designs
#--------------------------------------------------------#

library(car)

fit.aov <- aov(len ~ supp * dose, data = df)
fit.aov

class(fit.aov)
mode(fit.aov)
names(fit.aov)

fit.aov$coefficients
fit.aov$residuals
fit.aov$effects
fit.aov$rank
fit.aov$fitted.values
fit.aov$assign
fit.aov$qr
fit.aov$df.residual
fit.aov$contrasts
fit.aov$xlevels
fit.aov$call
fit.aov$terms
fit.aov$model
fit.aov$model == df


?Anova

library(car)

Anova(fit.aov, type = "III")
Anova(fit.aov, type = 3)

Anova(fit.aov, type = "II")
Anova(fit.aov, type = 2)

