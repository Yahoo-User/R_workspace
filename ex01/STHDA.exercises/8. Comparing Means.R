#--------------------------------------------------------#
# 1. Comparing the means of two independent groups
#--------------------------------------------------------#
# Unpaired Two-Samples Wilcoxon Test (non-parametric)
#--------------------------------------------------------#


#--------------------------------------------------------#
# 1. Import your data into R
#--------------------------------------------------------#

?rep

rep(1:4, 2)
rep(1:4, each = 2)

rep(1:4, c(2,2,2,2))
rep(1:4, c(2,1,2,1))

rep(1:4, each = 2, len = 4) 
rep(1:4, each = 2, len = 10)

rep(1:4, each = 2, times = 3)

rep( c("Woman", "Man"), 9 )
rep( c("Woman", "Man"), each= 9 )


women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
men_weight <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4)

table(men_weight)
table(women_weight)

df <- data.frame( 
  group = rep( c("Woman", "Man"), each = 9 ),
  weight = c(women_weight,  men_weight)
)
df



#--------------------------------------------------------#
# 2. Check your data
#--------------------------------------------------------#

print(df)


library(dplyr)

group_by(df, group) %>% 
  summarise(
    count = n(),
    median = median(weight, na.rm = T),
    IQR = IQR(weight, na.rm = T)
  )


library(ggpubr)

desc_statby(data = df,  measure.var = 'weight', grps = 'group', ci = .95)
desc_statby(df, 'weight', 'group', .99)



#--------------------------------------------------------#
# 3. Visualize your data using box plots
#--------------------------------------------------------#

library(ggpubr)


?ggboxplot

ggboxplot(df, x = 'group', y = 'weight')

ggboxplot(df, x = 'group', y = 'weight', size = 1)
ggboxplot(df, x = 'group', y = 'weight', size = .5)

ggboxplot(df, x = 'group', y = 'weight', outlier.shape = 19)
ggboxplot(df, x = 'group', y = 'weight', outlier.shape = 18)
ggboxplot(df, x = 'group', y = 'weight', outlier.shape = 17)
ggboxplot(df, x = 'group', y = 'weight', outlier.shape = 16)
ggboxplot(df, x = 'group', y = 'weight', outlier.shape = 15)
ggboxplot(df, x = 'group', y = 'weight', outlier.shape = 14)
ggboxplot(df, x = 'group', y = 'weight', outlier.shape = 13)
ggboxplot(df, x = 'group', y = 'weight', outlier.shape = 12)
ggboxplot(df, x = 'group', y = 'weight', outlier.shape = 11)
ggboxplot(df, x = 'group', y = 'weight', outlier.shape = 10)

ggboxplot(df, x = 'group', y = 'weight', select = 'after')
ggboxplot(df, x = 'group', y = 'weight', select = 'before')

ggboxplot(df, x = 'group', y = 'weight', remove = 'after')
ggboxplot(df, x = 'group', y = 'weight', remove = 'before')

ggboxplot(df, x = 'group', y = 'weight', order = c('before', 'after'))
ggboxplot(df, x = 'group', y = 'weight', order = c('after', 'before'))

ggboxplot(df, x = "group", y = 'weight', fill = "group")
ggboxplot(df, x = "group", y = 'weight', fill = c("skyblue", "lightgray"))

ggboxplot(df, x = 'group', y = 'weight', width = 0.5)
ggboxplot(df, x = 'group', y = 'weight', width = 0.2)
ggboxplot(df, x = 'group', y = 'weight', width = 0.7)

ggboxplot(df, x = 'group', y = 'weight', notch = T)

ggboxplot(df, x = 'group', y = 'weight', add = 'none')
ggboxplot(df, x = 'group', y = 'weight', add = 'dotplot')
ggboxplot(df, x = 'group', y = 'weight', add = 'jitter')
ggboxplot(df, x = 'group', y = 'weight', add = 'boxplot')
ggboxplot(df, x = 'group', y = 'weight', add = 'point')
ggboxplot(df, x = 'group', y = 'weight', add = 'mean')
ggboxplot(df, x = 'group', y = 'weight', add = 'mean_se')
ggboxplot(df, x = 'group', y = 'weight', add = 'mean_sd')
ggboxplot(df, x = 'group', y = 'weight', add = 'mean_ci')
ggboxplot(df, x = 'group', y = 'weight', add = 'mean_range')
ggboxplot(df, x = 'group', y = 'weight', add = 'median')
ggboxplot(df, x = 'group', y = 'weight', add = 'median_iqr')
ggboxplot(df, x = 'group', y = 'weight', add = 'median_hilow')
ggboxplot(df, x = 'group', y = 'weight', add = 'median_q1q3')
ggboxplot(df, x = 'group', y = 'weight', add = 'median_mad')
ggboxplot(df, x = 'group', y = 'weight', add = 'median_range')

ggboxplot(df, x = 'group', y = 'weight', ggtheme = theme())
ggboxplot(df, x = 'group', y = 'weight', ggtheme = theme_bw())
ggboxplot(df, x = 'group', y = 'weight', ggtheme = theme_classic())
ggboxplot(df, x = 'group', y = 'weight', ggtheme = theme_classic2())
ggboxplot(df, x = 'group', y = 'weight', ggtheme = theme_cleveland())
ggboxplot(df, x = 'group', y = 'weight', ggtheme = theme_get())
ggboxplot(df, x = 'group', y = 'weight', ggtheme = theme_dark())
ggboxplot(df, x = 'group', y = 'weight', ggtheme = theme_gray())
ggboxplot(df, x = 'group', y = 'weight', ggtheme = theme_light())
ggboxplot(df, x = 'group', y = 'weight', ggtheme = theme_linedraw())
ggboxplot(df, x = 'group', y = 'weight', ggtheme = theme_minimal())
ggboxplot(df, x = 'group', y = 'weight', ggtheme = theme_pubclean())
ggboxplot(df, x = 'group', y = 'weight', ggtheme = theme_pubr())
ggboxplot(df, x = 'group', y = 'weight', ggtheme = theme_replace())
ggboxplot(df, x = 'group', y = 'weight', ggtheme = theme_set())
ggboxplot(df, x = 'group', y = 'weight', ggtheme = theme_test())
ggboxplot(df, x = 'group', y = 'weight', ggtheme = theme_transparent())
ggboxplot(df, x = 'group', y = 'weight', ggtheme = theme_update())
ggboxplot(df, x = 'group', y = 'weight', ggtheme = theme_void())

ggboxplot(df, x = 'group', y = 'weight', xlab = F, ylab = F)
ggboxplot(df, x = 'group', y = 'weight', xlab = 'Groups', ylab = 'Weight(g)')

ggboxplot(df, x = 'group', y = 'weight', 
          color = 'group', palette = c("#00AFBB", "#E7B800"), ylab = 'Weight', xlab = 'Groups')



#--------------------------------------------------------#
# 4. Compute unpaired two-samples Wilcoxon test
#--------------------------------------------------------#

?wilcox.test

res <- wilcox.test(women_weight, men_weight)
res <- wilcox.test(women_weight, men_weight, alternative = 'two.sided')
res <- wilcox.test(women_weight, men_weight, alternative = 'left')
res <- wilcox.test(women_weight, men_weight, alternative = 'greater')
res

res <- wilcox.test(weight ~ group, data = df)
res <- wilcox.test(weight ~ group, data = df, alternative = 'two.sided')
res <- wilcox.test(weight ~ group, data = df, alternative = 'left')
res <- wilcox.test(weight ~ group, data = df, alternative = 'greater')
res

class(res)
mode(res)
names(res)

res$statistic
res$parameter
res$p.value
res$null.value
res$alternative
res$method
res$data.name
