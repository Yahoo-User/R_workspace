#----------------------------------------------------------------------------------------#
# Correlation Test: To evaluate the association between two or more variables.
#----------------------------------------------------------------------------------------#
# For instance, if we are interested to know whether there is a relationship 
# between the heights of fathers and sons, a correlation coefficient can be calculated
# to answer this question.
#
#----------------------------------------------------------------------------------------#
# ** Notice **
# If there is no relationship between two variables (ex: father and son's heights),
# the average height of son should be the same, regardless of the height of the
# fathers and vice versa.
#----------------------------------------------------------------------------------------#


#----------------------------------------------------------------------------------------
# 1. To install required packages for correlation analysis.
#----------------------------------------------------------------------------------------
# install.packages('ggpubr', dependencies = T)


#----------------------------------------------------------------------------------------
# 2. To load libraries for correlation analysis.
#----------------------------------------------------------------------------------------
library(ggpubr)  # for a scatter plot
library(dplyr)   # for data munging


#----------------------------------------------------------------------------------------
# 3. To load sample data set from default {datasets} mtcars.
#----------------------------------------------------------------------------------------
df_mtcars <- as.data.frame(datasets::mtcars, stringsAsFactors = F)
mpg_wt <- df_mtcars %>% select(mpg, wt)

class(mpg_wt)
head(mpg_wt, 6)


#----------------------------------------------------------------------------------------
# 4. To visualize sample data using scatter plot {ggpubr} ggscatter function.
#----------------------------------------------------------------------------------------
ggscatter(
  data = mpg_wt, 
  x = 'mpg', y = 'wt', 
  
  # "none", "reg.line" (linear regression line), "loess" (local regression fitting)
  add = 'loess',
  
  conf.int = T, cor.coef = T, cor.method = 'pearson', 
  xlab = 'Miles per gallon', ylab = 'Weight (1000 lbs)'
)


#----------------------------------------------------------------------------------------
# 5. To test the normality of both two variables (mpg, wt) of given data
#    using the "Shapiro-Wilk Normality Test" {stats::shapiro.test}:
#       - Null hypothesis: the data (mpg, wt) is normally distributed.
#       - Alternative hypothesis: the data (mpg, wt) is NOT normally distributed.
#    , and visualize the normality of two variables (mpg, wt) using {ggpubr::ggqqplot}
#----------------------------------------------------------------------------------------

shapiro.test(x = mpg_wt$mpg)  # For mpg variable
shapiro.test(x = mpg_wt$wt)   # For wt variable

# Q-Q plot(Quantile-Quantile plot) for mpg variable to check the normality.
ggqqplot(data = mpg_wt$mpg, ylab = "Miles per gallon")

# Q-Q plot(Quantile-Quantile plot) for mpg variable to check the normality.
ggqqplot(data = mpg_wt$wt, ylab = "Weight (1000 lbs)")


#----------------------------------------------------------------------------------------
# 6. Correlation test between two variables(mpg, wt) of the data.
#----------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------
# 6-1. Parametric Test.
#----------------------------------------------------------------------------------------
#      Pearson Corrleation Test if all variables come from 
#      the normal distribution by using {stats::cor.test}.
#----------------------------------------------------------------------------------------

# PPMC <- cor.test(
#            mpg_wt$mpg, mpg_wt$wt,
#            method = 'pearson',
#            alternative = 'less'    # default, one-tailed test: 'greater', 'less'
#         )

PPMC <- cor.test(
  mpg_wt$mpg, mpg_wt$wt,
  method = 'pearson'
)

#----------------------------------------------------------------------------------------
# t                   : "t-test" statistic value
# df                  : the degrees of freedom
# p-value             : the significance level, "alpha" of the "t-test" : 5%
# sample estimates    : the correlation coefficient ( comprised between -1 and 1 )
# confidence interval : the conf.int of the correlation coefficient at 95%
#----------------------------------------------------------------------------------------
PPMC

typeof(PPMC)
str(PPMC)

PPMC$statistic
PPMC$p.value
PPMC$parameter
PPMC$estimate
PPMC$alternative
PPMC$method

#----------------------------------------------------------------------------------------
# 6-2. Non-parametric Test.
#----------------------------------------------------------------------------------------
#      Kendall rank correlation coefficient (also called, "Kendall's tau") is used 
#      to estimate a rank-based measure of association.
#      (especially, "tau" is the Kendall correlation coefficient)
#
#      This test may be used if the data do not necessarily come from 
#      a bivariate normal distribution by using {stats::cor.test}.
#----------------------------------------------------------------------------------------

# KRCT <- cor.test(
#             mpg_wt$mpg, mpg_wt$wt, 
#             method = 'kendall',
#             alternative = 'two.sided'   # default, two-tailed test: 'two.sided'
#         )

# KRCT <- cor.test(
#           mpg_wt$mpg, mpg_wt$wt, 
#           method = 'kendall'
#         )

# KRCT

# KRCT$statistic
# KRCT$p.value
# KRCT$parameter
# KRCT$estimate
# KRCT$alternative
# KRCT$method

#----------------------------------------------------------------------------------------
# 6-3. Non-parametric Test.
#----------------------------------------------------------------------------------------
#      Spearman's rank correlation rho is used to estimate a rank-based 
#      measure of association. 
#      (especially, "rho" is the Spearman rank correlation coefficient)
#
#      This test may be used if the data do not necessarily come from
#      a bivariate normal distribution by using {stats::cor.test}.
#----------------------------------------------------------------------------------------

# SRCC <- cor.test(
#           mpg_wt$mpg, mpg_wt$wt, 
#           method = 'spearman',
#           alternative = 'two.sided'     # default, two-tailed test: 'two.sided'
#         )

# SRCC <- cor.test(
#           mpg_wt$mpg, mpg_wt$wt, 
#           method = 'spearman'
#         )

# SRCC

# SRCC$statistic
# SRCC$p.value
# SRCC$parameter
# SRCC$estimate
# SRCC$alternative
# SRCC$method

