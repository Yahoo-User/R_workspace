library(ggpubr)  # for a scatter plot
library(dplyr)   # for data munging

df_mtcars <- as.data.frame(datasets::mtcars, stringsAsFactors = F)
df_eco <- as.data.frame(ggplot2::economics, stringsAsFactors = F)

View(df_eco)


#------------------------------------------------------------

ggscatter(
  data = df_mtcars, 
  x = 'wt', y = 'mpg', 
  
  # add = 'reg.line',  
  add = 'loess', 
  
  conf.int = T, cor.coef = T, cor.method = 'pearson', 
  xlab = 'Weight (1000 lbs)', ylab = 'Miles Per Gallon'
) # mpg / wt

ggscatter(
  data = df_mtcars, 
  x = 'disp', y = 'mpg', 
  
  # add = 'reg.line',  
  add = 'loess', 
  
  conf.int = T, cor.coef = T, cor.method = 'pearson', 
  xlab = 'Displacement', ylab = 'Miles Per Gallon'
) # mpg / disp

#------------------------------------------------------------

ggscatter(
  data = df_eco, 
  x = 'unemploy', y = 'psavert', 
  
  # add = 'reg.line',  
  add = 'loess', 
  
  conf.int = T, cor.coef = T, cor.method = 'pearson', 
  xlab = 'No. of unemployed (1000)', ylab = 'Personal Savings Rate'
) # psavert / unemploy

ggscatter(
  data = df_eco, 
  x = 'uempmed', y = 'psavert', 
  
  # add = 'reg.line',  
  add = 'loess', 
  
  conf.int = T, cor.coef = T, cor.method = 'pearson', 
  xlab = 'Median Duration Of unemployment (1 week)', ylab = 'Personal Savings Rate'
) # psavert / uempmed

ggscatter(
  data = df_eco, 
  x = 'unemploy', y = 'pce', 
  
  # add = 'reg.line',  
  add = 'loess', 
  
  conf.int = T, cor.coef = T, cor.method = 'pearson', 
  xlab = 'No. of unemployed (1000)', ylab = 'Personal Consumption Expenditures (USD$1 Billion)'
) # pce / unemploy

ggscatter(
  data = df_eco, 
  x = 'uempmed', y = 'pce', 
  
  # add = 'reg.line',  
  add = 'loess', 
  
  conf.int = T, cor.coef = T, cor.method = 'pearson', 
  xlab = 'Median Duration Of unemployment (1 week)', ylab = 'Personal Consumption Expenditures (USD$1 Billion)'
) # pce / uempmed

ggscatter(
  data = df_eco, 
  x = 'pop', y = 'pce', 
  
  # add = 'reg.line',  
  add = 'loess', 
  
  conf.int = T, cor.coef = T, cor.method = 'pearson', 
  xlab = 'Total Population (1000)', ylab = 'Personal Consumption Expenditures (USD$1 Billion)'
) # pce / pop

ggscatter(
  data = df_eco, 
  x = 'pop', y = 'psavert', 
  
  # add = 'reg.line',  
  add = 'loess', 
  
  conf.int = T, cor.coef = T, cor.method = 'pearson', 
  xlab = 'Total Population (1000)', ylab = 'Personal Savings Rate'
) # psavert / pop

ggscatter(
  data = df_eco, 
  x = 'unemploy', y = 'uempmed', 
  
  # add = 'reg.line',  
  add = 'loess', 
  
  conf.int = T, cor.coef = T, cor.method = 'pearson', 
  xlab = 'No. of unemployed (1000)', ylab = 'Median Duration Of unemployment (1 week)'
) # uempmed / unemploy

ggscatter(
  data = df_eco, 
  x = 'uempmed', y = 'psavert',  
  
  # add = 'reg.line',  
  add = 'loess', 
  
  conf.int = T, cor.coef = T, cor.method = 'pearson', 
  xlab = 'Median Duration Of unemployment (1 week)', ylab = 'Personal Savings Rate'
) # pop / date

#------------------------------------------------------------
df_eco2 <- df_eco[,-1]
str(df_eco2)

chart.Correlation(df_eco2, histogram = T, pch = 19)
