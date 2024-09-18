#--------------------------------------------------------#
# 1. Install required R packages
#--------------------------------------------------------#

# install.packages('dplyr')
# install.packages('ggpubr')


#--------------------------------------------------------#
# 2. Load required R packages
#--------------------------------------------------------#

library(dplyr)
library(ggpubr)


#--------------------------------------------------------#
# 3. Import your data into R
#--------------------------------------------------------#

?ToothGrowth
head(ToothGrowth)
dim(ToothGrowth)
str(ToothGrowth)


#--------------------------------------------------------#
# 4. Check your data
#--------------------------------------------------------#

?set.seed
set.seed(1234)

?dplyr::sample_n
dplyr::sample_n(ToothGrowth, 10)
dplyr::sample_n(ToothGrowth, 10, replace = TRUE)


#--------------------------------------------------------#
# 5. Assess the normality of the data in R
#--------------------------------------------------------#

# 5-1. Visual methods
library(ggpubr)

ggdensity(ToothGrowth$len)
ggdensity(ToothGrowth$len, main = '- Density plot of tooth length -')
ggdensity(ToothGrowth$len, main = '- Density plot of tooth length -', linetype = 'dashed')
ggdensity(ToothGrowth$len, main = '- Density plot of tooth length -', linetype = 'dashed', color = 'blue')
ggdensity(ToothGrowth$len, main = '- Density plot of tooth length -', fill = 'skyblue')
ggdensity(ToothGrowth$len, title = '- Density plot of tooth length -', xlab = 'Tooth length', ylab = 'Density')

# 5-2. Q-Q plot
library(ggpubr)

ggqqplot(ToothGrowth$len)


library(car)

?qqPlot
qqPlot(ToothGrowth$len)
qqPlot(ToothGrowth$len, simulate = FALSE)   # 사용되지 않은 인자 (simulate = FALSE)
qqPlot(ToothGrowth$len, reps = 10)          # 사용되지 않은 인자 (reps = 10)
qqPlot(ToothGrowth$len, id = FALSE)
qqPlot(ToothGrowth$len, id = TRUE)
qqPlot(ToothGrowth$len, lwd = 1)
qqPlot(ToothGrowth$len, lwd = 1, pch = 1)
qqPlot(ToothGrowth$len, lwd = 1, pch = 2)
qqPlot(ToothGrowth$len, lwd = 1, pch = 3)
qqPlot(ToothGrowth$len, lwd = 1, pch = 4)
qqPlot(ToothGrowth$len, lwd = 1, pch = 5)
qqPlot(ToothGrowth$len, lwd = 1, pch = 6)
qqPlot(ToothGrowth$len, lwd = 1, pch = 7)
qqPlot(ToothGrowth$len, lwd = 1, pch = 8)
qqPlot(ToothGrowth$len, lwd = 1, pch = 9)
qqPlot(ToothGrowth$len, lwd = 1, pch = 10)
qqPlot(ToothGrowth$len, lwd = 1, pch = 11)
qqPlot(ToothGrowth$len, lwd = 1, pch = 12)
qqPlot(ToothGrowth$len, lwd = 1, pch = 13)
qqPlot(ToothGrowth$len, lwd = 1, pch = 14)
qqPlot(ToothGrowth$len, lwd = 1, pch = 15)
qqPlot(ToothGrowth$len, lwd = 1, pch = 16)
qqPlot(ToothGrowth$len, lwd = 1, pch = 17)
qqPlot(ToothGrowth$len, lwd = 1, pch = 18)
qqPlot(ToothGrowth$len, lwd = 1, pch = 19)
qqPlot(ToothGrowth$len, lwd = 1, pch = 20)
qqPlot(ToothGrowth$len, lwd = 1, pch = 21)
qqPlot(ToothGrowth$len, lwd = 1, pch = 22)
qqPlot(ToothGrowth$len, lwd = 1, pch = 23)
qqPlot(ToothGrowth$len, lwd = 1, pch = 24)
qqPlot(ToothGrowth$len, lwd = 1, pch = 24, cex = 1)
qqPlot(ToothGrowth$len, lwd = 1, pch = 24, cex = 2)
qqPlot(ToothGrowth$len, lwd = 1, pch = 24, cex = 3)
qqPlot(ToothGrowth$len, lwd = 2, envelope = .95)
qqPlot(ToothGrowth$len, lwd = 2, envelope = .99)
qqPlot(ToothGrowth$len, lwd = 2, envelope = .99, col = 'red')
qqPlot(ToothGrowth$len, lwd = 2, envelope = .99, col = 'red', col.lines = 'black')
qqPlot(ToothGrowth$len, main='Q-Q plot of Tooth length')
qqPlot(ToothGrowth$len, main='Q-Q plot of Tooth length', xlab = 'Theoretical', ylab = 'Length')
qqPlot(ToothGrowth$len, main='Q-Q plot of Tooth length', xlab = 'Theoretical', ylab = 'Length', las = 0)
qqPlot(ToothGrowth$len, main='Q-Q plot of Tooth length', xlab = 'Theoretical', ylab = 'Length', las = 1)


#--------------------------------------------------------#
# 6. Normality test
#--------------------------------------------------------#

?shapiro.test
shapiro.test(ToothGrowth$len)  # between 3 and 5000.
