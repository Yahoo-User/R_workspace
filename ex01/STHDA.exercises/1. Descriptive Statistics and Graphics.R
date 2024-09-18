#--------------------------------------------------------#
# 1. Import your data into R
#--------------------------------------------------------#

df <- read.delim(file.choose(), sep = ',')
df_csv <- read.csv(file.choose(), sep = ',')


#--------------------------------------------------------#
# 2. Check your data
#--------------------------------------------------------#

head(df_csv)
head(df_csv, 6)
head(df_csv, 20)


#--------------------------------------------------------#
# 3. R functions for computing descriptive statistics
#--------------------------------------------------------#

mean(iris$Sepal.Length)
sd(iris$Sepal.Length)
var(iris$Sepal.Length)
min(iris$Sepal.Length)
max(iris$Sepal.Length)
median(iris$Sepal.Length)
range(iris$Sepal.Length)

quantile(iris$Sepal.Length)
quantile(iris$Sepal.Length, probs = seq(0, 1, .25))
quantile(iris$Sepal.Length, seq(0, 1, .25))
quantile(iris$Sepal.Length, seq(0, 1, .1))
quantile(iris$Sepal.Length, seq(0, 1, .05))

summary(iris$Sepal.Length)
summary(iris)

IQR(iris$Sepal.Length)


# install.packages('modeest', dependencies = T)
library(modeest)
modeest::mfv(iris$Sepal.Length)

library(statip)
statip::mfv(iris$Sepal.Length)


#--------------------------------------------------------#
# 4. Descriptive statistics for a single group
#--------------------------------------------------------#

# 4-1. Measure of central tendency: mean, median, mode
mean(iris$Sepal.Length)
median(iris$Sepal.Length)
modeest::mfv(iris$Sepal.Length)
statip::mfv(iris$Sepal.Length)

# 4-2. Measure of variablity
min(iris$Sepal.Length)
max(iris$Sepal.Length)
range(iris$Sepal.Length)

# 4-3. Interquartile range
quantile(iris$Sepal.Length)
quantile(iris$Sepal.Length, seq(0, 1, .1))
IQR(iris$Sepal.Length)

# 4-4. Variance and standard deviation
var(iris$Sepal.Length)
sd(iris$Sepal.Length)

# 4-5. Median absolute deviation
median(iris$Sepal.Length)
mad(iris$Sepal.Length)


#--------------------------------------------------------#
# 5. Computing an overall summary of a variable and an entire data frame
#--------------------------------------------------------#

# 5-1. summary() function
summary(iris)
summary(iris, digits = 1)
summary(iris, digits = 2)
summary(iris, digits = 3)
summary(iris, digits = 6)
summary(iris, digits = 10)
summary(iris, digits = 30)
summary(iris, digits = 20)

# 5-2. sapply() function
sapply(iris, mean)
sapply(iris[-length(iris)], mean)
sapply(iris[-length(iris)], sd)
sapply(iris[-length(iris)], quantile)
sapply(iris[-length(iris)], quantile, probs=seq(0, 1, .01))
sapply(iris[-length(iris)], quantile, probs=seq(0, 1, .25))

# 5-3. stat.desc{pastecs} function
# install.packages("pastecs")
library(pastecs)

stat.desc(iris)
stat.desc(iris[-length(iris)])
round(stat.desc(iris[-length(iris)]), 2)

# 5-4. Case of missing values
mean(iris$Sepal.Length, na.rm = T)


#--------------------------------------------------------#
# 6. Graphical display of distributions
#--------------------------------------------------------#

# 6-1. Installation and loading ggpubr
library(ggpubr)

# install.packages("devtools")

res <- require(devtools)
res
class(res)

if(!require(devtools)) install.packages('ggpubr')

res <- library(ggpubr)
res
class(res)
mode(res)
length(res)

# 6-2. Box plots
library(ggpubr)
ggboxplot(iris, y="Sepal.Length")
ggboxplot(iris, y='Sepal.Length', width = .5)
ggboxplot(iris, y='Sepal.Length', width = 1)
ggboxplot(iris, y='Sepal.Length', width = .5)
ggboxplot(iris, y='Sepal.Length', width = .3)
ggboxplot(iris, y='Sepal.Length', width = .2)
ggboxplot(iris, y='Sepal.Length', width = .1)
ggboxplot(iris, y='Sepal.Length', width = .01)
qqboxplot(iris, x='Species', y='Sepal.Length', width=.5)
ggboxplot(iris, x='Species', y='Sepal.Length', width = .5)
ggboxplot(iris, x='Species', y='Sepal.Length', width = 1)
ggboxplot(iris, x='Species', y='Sepal.Length', width = .3)
ggboxplot(iris, x='Species', y='Sepal.Length', width = .2)
ggboxplot(iris, x='Species', y='Sepal.Length', width = .1)

# 6-3. Histogram
gghistogram(iris, x='Sepal.Length')
gghistogram(iris, x='Sepal.Length', bins = 9)
gghistogram(iris, x='Sepal.Length', bins = 9, add = 'mean')
gghistogram(data = iris, x = 'Sepal.Length', bins = 30, add = 'mean')
gghistogram(iris, x = 'Sepal.Length', bins = 30, color = 'red')
gghistogram(iris, x = 'Sepal.Length', bins = 30, fill = 'blue')
gghistogram(iris, x = 'Sepal.Length', bins = 30, color = 'red', fill = 'skyblue')
gghistogram(iris, x = 'Sepal.Length', bins = 30, color = 'red', fill = 'skyblue', title = '- Histogram of Sepal.Length - ')
gghistogram(iris, x = 'Sepal.Length', bins = 30, color = 'red', fill = 'skyblue', 
            title = '- Histogram of Sepal.Length - ', xlab = 'Sepal.Length', ylab = 'Frequency')

# 6-4. Empirical cumulative distribution function (ECDF)
ggecdf(iris, x = 'Sepal.Length')
ggecdf(iris, x = 'Sepal.Length', color = 'red', linetype = 'dashed')

# 6-5. Q-Q plots
ggqqplot(iris, x = 'Sepal.Length')
ggqqplot(ggplot2::mpg, x = 'cty')
ggqqplot(ggplot2::mpg, x = 'hwy')


#--------------------------------------------------------#
# 7. Descriptive statistics by groups
#--------------------------------------------------------#

# install.packages('dplyr', dependencies = T)
library(dplyr)

# 7-1. Descriptive statistics by groups
group_by(iris, Species) %>% 
  summarise(
    count = n(),
    mean = mean(Sepal.Length, na.rm = T),
    sd = sd(Sepal.Length, na.rm = T)
  )

# 7-2. Graphics for grouped data
library(ggpubr)

# Box plot colored by groups: Species
ggboxplot(iris, x = 'Species', y = 'Sepal.Length')
ggboxplot(iris, x = 'Species', y = 'Sepal.Length', color = 'Species')
ggboxplot(iris, x = 'Species', y = 'Sepal.Length', color = 'Species', palette = c("#00AFBB", "#E7B800", "#FC4E07"))

# Stripchart colored by groups: Species
ggstripchart(iris, x = 'Species', y = 'Sepal.Length')
ggstripchart(iris, x = 'Species', y = 'Sepal.Length', color = 'Species')
ggstripchart(iris, x = 'Species', y = 'Sepal.Length', color = 'Species', palette = c("#00AFBB", "#E7B800", "#FC4E07"))
ggstripchart(iris, x = 'Species', y = 'Sepal.Length', color = 'Species', palette = c("#00AFBB", "#E7B800", "#FC4E07"), add = 'mean_sd')


#--------------------------------------------------------#
# 8. Frequency tables
#--------------------------------------------------------#

?HairEyeColor
class(HairEyeColor)

df <- as.data.frame(HairEyeColor)
row.names(df)
colnames(df)
dim(df)
head(df)
nrow(df)

rep(1:3, 2)
rep(1, 10)
rep('string', 10)

rep(row.names(df), df$Freq)
df[ rep(row.names(df), df$Freq), 1:3]

hair_eye_color <- df[ rep(row.names(df), df$Freq), 1:3]
hair_eye_color

rownames(hair_eye_color) <- 1:nrow(hair_eye_color)

head(hair_eye_color)

Hair <- hair_eye_color$Hair
Eye  <- hair_eye_color$Eye

Hair
Eye

# 8-1. Simple frequency distribution: one categorical variable
table(Hair)
table(Eye)

df <- as.data.frame( table(Hair) )
df

library(ggpubr)
ggbarplot(df, x = 'Hair' , y = 'Freq')


# 8-2. Two-way contingency table: Two categorical variables
crosstbl <- table(Hair, Eye)
crosstbl
df_crosstbl1 <- as.data.frame(crosstbl)

crosstbl2 <- xtabs(~Hair+Eye, data = hair_eye_color)
crosstbl2
df_crosstbl2 <- as.data.frame(crosstbl2)

library(ggpubr)

ggbarplot(df_crosstbl1, x = 'Hair', y = 'Eye')
ggbarplot(df_crosstbl1, x = 'Hair', y = 'Eye', color = 'Eye')
ggbarplot(df_crosstbl1, x = 'Hair', y = 'Eye', color = 'Eye', palette = c("brown", "blue", "gold", "green"))
ggbarplot(df_crosstbl1, x = 'Hair', y = 'Eye', fill = 'Eye')
ggbarplot(df_crosstbl1, x = 'Hair', y = 'Eye', fill = 'Eye', palette = c("brown", "blue", "gold", "green"))

ggbarplot(df_crosstbl2, x = 'Hair', y = 'Eye')
ggbarplot(df_crosstbl2, x = 'Hair', y = 'Eye', color = 'Eye')
ggbarplot(df_crosstbl2, x = 'Hair', y = 'Eye', color = 'Eye', palette = c("brown", "blue", "gold", "green"))
ggbarplot(df_crosstbl2, x = 'Hair', y = 'Eye', fill = 'Eye')
ggbarplot(df_crosstbl2, x = 'Hair', y = 'Eye', fill = 'Eye', palette = c("brown", "blue", "gold", "green"))

ggbarplot(df_crosstbl2, x = 'Hair', y = 'Eye', position = position_dodge())
ggbarplot(df_crosstbl2, x = 'Hair', y = 'Eye', position = position_dodge(), color = 'Eye')
ggbarplot(df_crosstbl2, x = 'Hair', y = 'Eye', position = position_dodge(), color = 'Eye', palette = c("brown", "blue", "gold", "green"))
ggbarplot(df_crosstbl2, x = 'Hair', y = 'Eye', position = position_dodge(), fill = 'Eye')
ggbarplot(df_crosstbl2, x = 'Hair', y = 'Eye', position = position_dodge(), fill = 'Eye', palette = c("brown", "blue", "gold", "green"))


# 8-3. Multiway tables: More than two categorical variables
xtabs(~Hair+Eye+Sex, data = hair_eye_color)
?ftable
ftable(Sex + Hair ~ Eye, data = hair_eye_color)


# 8-4. Compute table margins and relative frequency
Hair <- hair_eye_color$Hair
Eye  <- hair_eye_color$Eye

he.tbl <- table(Hair, Eye)
he.tbl

margin.table(he.tbl, 1)
margin.table(he.tbl, 2)


prop.table(he.tbl, 1)
round(prop.table(he.tbl, 1), 2) * 100

he.tbl / sum(he.tbl)



