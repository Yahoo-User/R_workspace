#--------------------------------------------------------#
# 1. Comparing the means of more than two groups
#--------------------------------------------------------#
# Analysis of variance (ANOVA, parametric)
#--------------------------------------------------------#
#   (1) One-Way ANOVA Test
#--------------------------------------------------------#


#--------------------------------------------------------#
# 1. Import your data into R
#--------------------------------------------------------#

?PlantGrowth
dim(PlantGrowth)
head(PlantGrowth)
str(PlantGrowth)

table(PlantGrowth$group)

( df <- PlantGrowth )



#--------------------------------------------------------#
# 2. Check your data
#--------------------------------------------------------#

set.seed(1234)

dplyr::sample_n(df, 10)

levels(df$group)

df$group <- ordered(df$group, levels=c('ctrl', 'trt1', 'trt2'))

str(df)
levels(df$group)


library(dplyr)

group_by(df, group) %>% 
    summarise(
      count = n(),
      mean = mean(weight, na.rm = T),
      sd = sd(weight, na.rm = T)
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

ggboxplot(df, x = 'group', y = 'weight', select = 'ctrl')
ggboxplot(df, x = 'group', y = 'weight', select = c('ctrl', 'trt1'))
ggboxplot(df, x = 'group', y = 'weight', select = c('trt1', 'trt2'))
ggboxplot(df, x = 'group', y = 'weight', select = c('trt1', 'trt2', 'ctrl'))

ggboxplot(df, x = 'group', y = 'weight', remove = 'ctrl')
ggboxplot(df, x = 'group', y = 'weight', remove = c('ctrl', 'trt1'))
ggboxplot(df, x = 'group', y = 'weight', remove = c('trt1', 'trt2'))
ggboxplot(df, x = 'group', y = 'weight', remove = c('trt1', 'trt2', 'ctrl'))

ggboxplot(df, x = 'group', y = 'weight', order = c('trt1', 'trt2', 'ctrl'))
ggboxplot(df, x = 'group', y = 'weight', order = c('trt2', 'ctrl', 'trt1'))

ggboxplot(df, x = 'group', y = 'weight', fill = 'group')
ggboxplot(df, x = 'group', y = 'weight', fill = c('lightgray', 'skyblue', 'green'))

ggboxplot(df, x = 'group', y = 'weight', width = 1)
ggboxplot(df, x = 'group', y = 'weight', width = .7)
ggboxplot(df, x = 'group', y = 'weight', width = .5)
ggboxplot(df, x = 'group', y = 'weight', width = .2)

ggboxplot(df, x = 'group', y = 'weight', notch = 1)

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

ggboxplot(df, x = 'group', y = 'weight', fill = 'group', palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c('ctrl', 'trt1', 'trt2'), xlab = 'Treatment', ylab = 'Weight')


dev.off()

library(ggpubr)

?ggline
ggline(df, x = 'group', y = 'weight')

ggline(df, x = 'group', y = 'weight', facet.by = 'group')

ggline(df, x = 'group', y = 'weight', linetype = 'solid')
ggline(df, x = 'group', y = 'weight', linetype = 'dashed')
ggline(df, x = 'group', y = 'weight', linetype = 'dotted')

ggline(df, x = 'group', y = 'weight', plot_type = 'b')
ggline(df, x = 'group', y = 'weight', plot_type = 'l')
ggline(df, x = 'group', y = 'weight', plot_type = 'p')

ggline(df, x = 'group', y = 'weight', shape = 19)
ggline(df, x = 'group', y = 'weight', shape = 17)
ggline(df, x = 'group', y = 'weight', shape = 15)
ggline(df, x = 'group', y = 'weight', shape = 13)
ggline(df, x = 'group', y = 'weight', shape = 11)

ggline(df, x = 'group', y = 'weight', point.color = 'red')

ggline(df, x = 'group', y = 'weight', point.size = 2)
ggline(df, x = 'group', y = 'weight', point.size = 1)
ggline(df, x = 'group', y = 'weight', point.size = .5)

ggline(df, x = 'group', y = 'weight', size = 1)
ggline(df, x = 'group', y = 'weight', size = .5)

ggline(df, x = 'group', y = 'weight', add = "none")
ggline(df, x = 'group', y = 'weight', add = "dotplot")
ggline(df, x = 'group', y = 'weight', add = "jitter")
ggline(df, x = 'group', y = 'weight', add = "boxplot")
ggline(df, x = 'group', y = 'weight', add = "point")
ggline(df, x = 'group', y = 'weight', add = "mean")
ggline(df, x = 'group', y = 'weight', add = "mean_se")
ggline(df, x = 'group', y = 'weight', add = "mean_sd")
ggline(df, x = 'group', y = 'weight', add = "mean_ci")
ggline(df, x = 'group', y = 'weight', add = "mean_range")
ggline(df, x = 'group', y = 'weight', add = "median")
ggline(df, x = 'group', y = 'weight', add = "median_iqr")
ggline(df, x = 'group', y = 'weight', add = "median_hilow")
ggline(df, x = 'group', y = 'weight', add = "median_q1q3")
ggline(df, x = 'group', y = 'weight', add = "median_mad")
ggline(df, x = 'group', y = 'weight', add = "median_range")

ggline(df, x = 'group', y = 'weight', ggtheme = theme())
ggline(df, x = 'group', y = 'weight', ggtheme = theme_bw())
ggline(df, x = 'group', y = 'weight', ggtheme = theme_classic())
ggline(df, x = 'group', y = 'weight', ggtheme = theme_classic2())
ggline(df, x = 'group', y = 'weight', ggtheme = theme_cleveland())
ggline(df, x = 'group', y = 'weight', ggtheme = theme_get())
ggline(df, x = 'group', y = 'weight', ggtheme = theme_dark())
ggline(df, x = 'group', y = 'weight', ggtheme = theme_gray())
ggline(df, x = 'group', y = 'weight', ggtheme = theme_light())
ggline(df, x = 'group', y = 'weight', ggtheme = theme_linedraw())
ggline(df, x = 'group', y = 'weight', ggtheme = theme_minimal())
ggline(df, x = 'group', y = 'weight', ggtheme = theme_pubclean())
ggline(df, x = 'group', y = 'weight', ggtheme = theme_pubr())
ggline(df, x = 'group', y = 'weight', ggtheme = theme_replace())
ggline(df, x = 'group', y = 'weight', ggtheme = theme_set())
ggline(df, x = 'group', y = 'weight', ggtheme = theme_test())
ggline(df, x = 'group', y = 'weight', ggtheme = theme_transparent())
ggline(df, x = 'group', y = 'weight', ggtheme = theme_update())
ggline(df, x = 'group', y = 'weight', ggtheme = theme_void())

ggline(df, x = 'group', y = 'weight', select = 'ctrl')
ggline(df, x = 'group', y = 'weight', select = c('trt1', 'trt2'))
ggline(df, x = 'group', y = 'weight', select = c('ctrl', 'trt2'))

ggline(df, x = 'group', y = 'weight', remove = 'ctrl')
ggline(df, x = 'group', y = 'weight', remove = c('trt1', 'trt2'))
ggline(df, x = 'group', y = 'weight', remove = c('ctrl', 'trt2'))

ggline(df, x = 'group', y = 'weight', order = c("ctrl", "trt1", "trt2"))
ggline(df, x = 'group', y = 'weight', order = c("trt1", "trt2", "ctrl"))

ggline(df, x = 'group', y = 'weight', color = 'group')

ggline(df, x = 'group', y = 'weight', xlab = F, ylab = F)
ggline(df, x = 'group', y = 'weight', xlab = 'Groups', ylab = 'Weight(g)')

ggline(df, x = "group", y = "weight", 
       add = c("mean_se", "jitter"), 
       order = c("ctrl", "trt1", "trt2"),
       ylab = "Weight", xlab = "Treatment")


dev.off()

boxplot(weight ~ group, data = df)
boxplot(weight ~ group, data = df, notch = TRUE)
boxplot(weight ~ group, data = df, boxwex = 0.5)
boxplot(weight ~ group, data = df, add = TRUE)
boxplot(weight ~ group, data = df, main = '- Main Title -')
boxplot(weight ~ group, data = df, subset = group == 'ctrl')
boxplot(weight ~ group, data = df, horizontal = T)
boxplot(weight ~ group, data = df, frame = FALSE)
boxplot(weight ~ group, data = df, col = c("#00AFBB", "#E7B800", "#FC4E07"))
boxplot(weight ~ group, data = df, xlab = 'Treatment', ylab = 'Weight')

boxplot(weight ~ group, data = df,
        xlab = "Treatment", ylab = "Weight",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))


dev.off()

library(gplots)

plotmeans(weight ~ group, data = df)
plotmeans(weight ~ group, data = df, connect = F)
plotmeans(weight ~ group, data = df, ccol = 'green')
plotmeans(weight ~ group, data = df, barcol = 'red')
plotmeans(weight ~ group, data = df, barwidth = 1)
plotmeans(weight ~ group, data = df, barwidth = 2)
plotmeans(weight ~ group, data = df, barwidth = .5)
plotmeans(weight ~ group, data = df, col = 'red')
plotmeans(weight ~ group, data = df, text.n.label='size=')
plotmeans(weight ~ group, data = df, n.label = F)
plotmeans(weight ~ group, data = df, ci.label=T)
plotmeans(weight ~ group, data = df, p = .99)
plotmeans(weight ~ group, data = df, bars = FALSE)
plotmeans(weight ~ group, data = df, subset = group == 'trt2')
plotmeans(weight ~ group, data = df, frame = FALSE)
plotmeans(weight ~ group, data = df, frame = FALSE, xlab = 'Treatment', y = 'Weight')

plotmeans(weight ~ group, data = df, frame = FALSE,
          xlab = "Treatment", ylab = "Weight",
          main="Mean Plot with 95% CI") 



#--------------------------------------------------------#
# 4. Compute one-way ANOVA test
#--------------------------------------------------------#

( fit.aov <- aov(weight ~ group, data = df) )

class(fit.aov)
mode(fit.aov)
names(fit.aov)

fit.aov$coefficients
fit.aov$residuals
fit.aov$effects
fit.aov$rank
fit.aov$fitted.values
fit.aov$assign
fit.aov$df.residual
fit.aov$call
fit.aov$terms
fit.aov$model
fit.aov$qr
fit.aov$xlevels
fit.aov$contrasts

summary(fit.aov)
summary.aov(fit.aov)



#--------------------------------------------------------#
# 5. Multiple pairwise-comparison between the means of groups
#--------------------------------------------------------#

# 5-1. Tukey multiple pairwise-comparisons

TukeyHSD(fit.aov)


# 5-2. Multiple comparisons using multcomp package
library(multcomp)

summary( glht(fit.aov, linfct = mcp(group = 'Tukey')) )


# 5-3. Pairewise t-test

pairwise.t.test(df$weight, df$group, p.adjust.method = "BH")



#--------------------------------------------------------#
# 6. Check ANOVA assumptions
#--------------------------------------------------------#

# 6-1. Check the homogeneity of variance assumption - method1

dev.off()
op = par(mfrow=c(2,2))

plot(fit.aov)

par(op)


dev.off()
op = par(mfrow=c(2,3))

plot(fit.aov, 1)
plot(fit.aov, 2)
plot(fit.aov, 3)
plot(fit.aov, 4)
plot(fit.aov, 5)
plot(fit.aov, 6)

par(op)


# 6-2. Check the homogeneity of variance assumption - method2

library(car)
leveneTest(weight ~ group, data = df)


# 6-3. Check the homogeneity of variance assumption - method3
bartlett.test(weight ~ group, data = df)


# 6-4. ANOVA test with no assumption of equal variances - method4
#      (Welch one-way test)
oneway.test(weight ~ group, data = df)


# 6-5. Pairwise t-tests with no assumption of equal variances - method2
pairwise.t.test(df$weight, df$group, p.adjust.method = "BH", pool.sd = FALSE)


# 6-6. Check the normality assumption - method1
dev.off()

plot(fit.aov, 2)


# 6-7. Check the normality assumption - method2
( aov_residuals <- residuals(object = fit.aov) )

shapiro.test(aov_residuals)

