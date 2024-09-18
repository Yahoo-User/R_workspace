#--------------------------------------------------------#
# 1. Comparing the means of paired samples
#--------------------------------------------------------#
# Paired Samples Wilcoxon Test (non-parametric)
# (Wilcoxon signed-rank test)
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


before <- c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
after  <- c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)

df <- data.frame( 
  group = rep(c("before", "after"), each = 10),
  weight = c(before,  after)
)
df

table(df$group)



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

dev.off()

ggboxplot(df, x = 'group', y = 'weight')

ggboxplot(df, x = 'group', y = 'weight', facet.by = 'group')

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

ggboxplot(df, x = 'group', y = 'weight', order = c("before", "after"),
          color = 'group', palette = c("#00AFBB", "#E7B800"), ylab = 'Weight', xlab = 'Groups')


?plot.paired
?subset

before <- subset(x = df, subset = (group == 'before'), select = c(group, weight), drop = T)
before <- subset(df, group == 'before', weight, drop = T)
before

after  <- subset(df, group == 'after', weight, drop = T)
after

library(PairedData)
pd <- paired(before, after)
pd

class(pd)
mode(pd)
names(pd)
pd$before
pd$after

plot(pd, type = 'profile')
plot(pd, type = 'profile') + theme()
plot(pd, type = 'profile') + theme_bw()
plot(pd, type = 'profile') + theme_classic()
plot(pd, type = 'profile') + theme_classic2()
plot(pd, type = 'profile') + theme_cleveland()
plot(pd, type = 'profile') + theme_get()
plot(pd, type = 'profile') + theme_dark()
plot(pd, type = 'profile') + theme_gray()
plot(pd, type = 'profile') + theme_light()
plot(pd, type = 'profile') + theme_linedraw()
plot(pd, type = 'profile') + theme_minimal()
plot(pd, type = 'profile') + theme_pubclean()
plot(pd, type = 'profile') + theme_pubr()
plot(pd, type = 'profile') + theme_replace()
plot(pd, type = 'profile') + theme_set()
plot(pd, type = 'profile') + theme_test()
plot(pd, type = 'profile') + theme_transparent()
plot(pd, type = 'profile') + theme_update()
plot(pd, type = 'profile') + theme_void()



#--------------------------------------------------------#
# 4. Compute paired-sample Wilcoxon test
#--------------------------------------------------------#

fit <- wilcox.test(before, after, paired = T)
fit <- wilcox.test(before, after, paired = T, alternative = 'two.sided')
fit <- wilcox.test(before, after, paired = T, alternative = 'less')
fit <- wilcox.test(before, after, paired = T, alternative = 'greater')
fit

fit <- wilcox.test(weight ~ group, data = df, paired = T)
fit <- wilcox.test(weight ~ group, data = df, paired = T, alternative = 'two.sided')
fit <- wilcox.test(weight ~ group, data = df, paired = T, alternative = 'less')
fit <- wilcox.test(weight ~ group, data = df, paired = T, alternative = 'greater')
fit

class(fit)
mode(fit)
names(fit)

fit$statistic
fit$parameter
fit$p.value
fit$null.value
fit$alternative
fit$method
fit$data.name
