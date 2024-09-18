#--------------------------------------------------------#
# 1. Comparing the means of more than two groups
#--------------------------------------------------------#
# Analysis of variance (ANOVA, parametric)
#--------------------------------------------------------#
#   (3) MANOVA Test: Multivariate Analysis of Variance
#--------------------------------------------------------#


#--------------------------------------------------------#
# 1. Import your data into R
#--------------------------------------------------------#

?iris

dim(iris)
str(iris)
View(iris)

df <- iris
head(df, 10)



#--------------------------------------------------------#
# 2. Check your data
#--------------------------------------------------------#

set.seed(1234)

dplyr::sample_n(df, 10)

table(iris$Species)
summary(iris)


library(ggpubr)

desc_statby(data = df,  measure.var = 'Sepal.Length', grps = 'Species', ci = .95)
desc_statby(df, 'Petal.Length', 'Species', .99)



#--------------------------------------------------------#
# 3. Compute MANOVA test
#--------------------------------------------------------#

Sepal.Length <- df$Sepal.Length
Petal.Length <- df$Petal.Length

res <- cbind(Sepal.Length, Petal.Length)
class(res)


?manova

( fit.mov <- manova( cbind(Sepal.Length, Petal.Length) ~ Species, data = df ) )

class(fit.mov)
mode(fit.mov)
names(fit.mov)

fit.mov$coefficients
fit.mov$residuals
fit.mov$effects
fit.mov$rank
fit.mov$fitted.values
fit.mov$assign
fit.mov$qr
fit.mov$df.residual
fit.mov$contrasts
fit.mov$xlevels
fit.mov$call
fit.mov$terms
fit.mov$model

summary(fit.mov)
summary.aov(fit.mov)
