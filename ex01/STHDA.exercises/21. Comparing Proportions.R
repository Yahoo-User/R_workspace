#--------------------------------------------------------#
# 1. Comparing Proportions
#--------------------------------------------------------#
# (4) Chi-Square Test of Independence
#--------------------------------------------------------#
# Target: Evaluate The Association Between Two Categorical Variables.
#--------------------------------------------------------#

# The chi-square test of independence is used to analyze the frequency table (i.e. contengency table) 
# formed by two categorical variables.
# The chi-square test evaluates whether there is a significant association between the categories of the two variables.



#--------------------------------------------------------#
# 1. Import and check your data
#--------------------------------------------------------#

( housetasks <- read.delim('./Data/housetasks.txt', sep = '\t') )

class(housetasks)
str(housetasks)


#--------------------------------------------------------#
# 2. Graphical display of contengency tables
#--------------------------------------------------------#

library(gplots)

( dt <- as.table(as.matrix(housetasks)) )

class(dt)


dev.off()

?t
?balloonplot

balloonplot(dt)

balloonplot(t(dt))

balloonplot(t(dt), show.margins = F)
balloonplot(t(dt), show.margins = T)

balloonplot(t(dt), label = T)
balloonplot(t(dt), label = F)

balloonplot(t(dt), xlab = '', ylab = '')

balloonplot(t(dt), main = 'housetasks', xlab = '', ylab = '', label = F, show.margins = F)
balloonplot(t(dt), main = 'housetasks', xlab = '', ylab = '', label = T, show.margins = T)


dev.off()

?mosaicplot

mosaicplot(dt)

mosaicplot(t(dt))

mosaicplot(dt, shade = T)
mosaicplot(dt, shade = F)

mosaicplot(dt, las = 1)
mosaicplot(dt, las = 2)
mosaicplot(dt, las = 3)


mosaicplot(dt, shade = F, las = 2, main = 'housetasks')
mosaicplot(dt, shade = T, las = 2, main = 'housetasks')


dev.off()

library(vcd)

?assoc

assoc(dt)

assoc(dt, shade = T)
assoc(dt, shade = F)

assoc(head(dt, 5), shade = T)
assoc(head(dt, 5), shade = T, main = 'association plot', sub = 'housetasks')



#--------------------------------------------------------#
# 3. Compute chi-square test
#--------------------------------------------------------#
( chisq <- chisq.test(housetasks) )
class(chisq)
mode(chisq)
names(chisq)

chisq$statistic
chisq$parameter
chisq$p.value
chisq$method
chisq$data.name
chisq$observed
chisq$expected
chisq$residuals
chisq$stdres

round(chisq$expected, 2)
round(chisq$residuals, 3)



#--------------------------------------------------------#
# 4. Visualize Pearson residuals
#--------------------------------------------------------#

library(corrplot)

corrplot(chisq$residuals, is.corr = F)



#--------------------------------------------------------#
# 5. Compute & Visualize the contibution in percentage (%)
#--------------------------------------------------------#
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)

dev.off()

corrplot(contrib, is.corr = F)

corrplot(contrib, is.corr = F, tl.col = "red")
corrplot(contrib, is.corr = F, tl.col = "black")

corrplot(contrib, is.corr = F, tl.srt = 90)
corrplot(contrib, is.corr = F, tl.srt = 45)

corrplot(contrib, is.corr = F, type = "full")
corrplot(contrib, is.corr = F, type = "lower")
corrplot(contrib, is.corr = F, type = "upper")

corrplot(contrib, is.corr = F, method = "circle")
corrplot(contrib, is.corr = F, method = "square")
corrplot(contrib, is.corr = F, method = "ellipse")
corrplot(contrib, is.corr = F, method = "number")
corrplot(contrib, is.corr = F, method = "shade")
corrplot(contrib, is.corr = F, method = "color")
corrplot(contrib, is.corr = F, method = "pie")
