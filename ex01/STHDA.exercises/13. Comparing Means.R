#--------------------------------------------------------#
# 1. Comparing the means of more than two groups
#--------------------------------------------------------#
# Analysis of variance (ANOVA, parametric)
#--------------------------------------------------------#
#   (2) Two-Way ANOVA Test for balanced designs
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

( df <- ToothGrowth )



#--------------------------------------------------------#
# 2. Check your data
#--------------------------------------------------------#

set.seed(1234)

dplyr::sample_n(df, 10)

str(df)
table(df$dose)

df$dose <- factor(df$dose, levels = c(.5, 1, 2), labels = c('D0.5', 'D1', 'D2'))
df

str(df)
table(df$supp, df$dose)


library(ggpubr)

desc_statby(data = df,  measure.var = 'len', grps = 'dose', ci = .95)
desc_statby(df, 'len', 'dose', .99)



#--------------------------------------------------------#
# 3. Visualize your data
#--------------------------------------------------------#

?ggboxplot

dev.off()

library(ggpubr)

ggboxplot(df, x = 'dose', y = 'len')

ggboxplot(df, x = 'dose', y = 'len', size = 1)
ggboxplot(df, x = 'dose', y = 'len', size = .5)

ggboxplot(df, x = 'dose', y = 'len', outlier.shape = 19)
ggboxplot(df, x = 'dose', y = 'len', outlier.shape = 18)
ggboxplot(df, x = 'dose', y = 'len', outlier.shape = 17)
ggboxplot(df, x = 'dose', y = 'len', outlier.shape = 16)
ggboxplot(df, x = 'dose', y = 'len', outlier.shape = 15)
ggboxplot(df, x = 'dose', y = 'len', outlier.shape = 14)
ggboxplot(df, x = 'dose', y = 'len', outlier.shape = 13)
ggboxplot(df, x = 'dose', y = 'len', outlier.shape = 12)
ggboxplot(df, x = 'dose', y = 'len', outlier.shape = 11)
ggboxplot(df, x = 'dose', y = 'len', outlier.shape = 10)

ggboxplot(df, x = 'dose', y = 'len', select = '0.5')
ggboxplot(df, x = 'dose', y = 'len', select = c('0.5', '1'))
ggboxplot(df, x = 'dose', y = 'len', select = c('1', '2'))
ggboxplot(df, x = 'dose', y = 'len', select = c('0.5', '1', '2'))
ggboxplot(df, x = 'dose', y = 'len', select = c('1', '0.5', '2'))

ggboxplot(df, x = 'dose', y = 'len', remove = '0.5')
ggboxplot(df, x = 'dose', y = 'len', remove = c('0.5', '1'))
ggboxplot(df, x = 'dose', y = 'len', remove = c('1', '2'))
ggboxplot(df, x = 'dose', y = 'len', remove = c('1', '0.5', '2'))

ggboxplot(df, x = 'dose', y = 'len', order = c('1', '2', '0.5'))
ggboxplot(df, x = 'dose', y = 'len', order = c('1', '0.5', '2'))

ggboxplot(df, x = 'dose', y = 'len', fill = 'dose')
ggboxplot(df, x = 'dose', y = 'len', fill = c('lightgray', 'skyblue', 'green'))

ggboxplot(df, x = 'dose', y = 'len', width =  1)
ggboxplot(df, x = 'dose', y = 'len', width = .7)
ggboxplot(df, x = 'dose', y = 'len', width = .5)
ggboxplot(df, x = 'dose', y = 'len', width = .2)

ggboxplot(df, x = 'dose', y = 'len', notch = 1)

ggboxplot(df, x = 'dose', y = 'len', add = 'none')
ggboxplot(df, x = 'dose', y = 'len', add = 'dotplot')
ggboxplot(df, x = 'dose', y = 'len', add = 'jitter')
ggboxplot(df, x = 'dose', y = 'len', add = 'boxplot')
ggboxplot(df, x = 'dose', y = 'len', add = 'point')
ggboxplot(df, x = 'dose', y = 'len', add = 'mean')
ggboxplot(df, x = 'dose', y = 'len', add = 'mean_se')
ggboxplot(df, x = 'dose', y = 'len', add = 'mean_sd')
ggboxplot(df, x = 'dose', y = 'len', add = 'mean_ci')
ggboxplot(df, x = 'dose', y = 'len', add = 'mean_range')
ggboxplot(df, x = 'dose', y = 'len', add = 'median')
ggboxplot(df, x = 'dose', y = 'len', add = 'median_iqr')
ggboxplot(df, x = 'dose', y = 'len', add = 'median_hilow')
ggboxplot(df, x = 'dose', y = 'len', add = 'median_q1q3')
ggboxplot(df, x = 'dose', y = 'len', add = 'median_mad')
ggboxplot(df, x = 'dose', y = 'len', add = 'median_range')

ggboxplot(df, x = 'dose', y = 'len', fill = 'supp', palette = c("#00AFBB", "#E7B800"),
          order = c('0.5', '1', '2'), xlab = 'Dose', ylab = 'Length')


dev.off()

library(ggpubr)

?ggline
ggline(df, x = 'dose', y = 'len')

ggline(df, x = 'dose', y = 'len', facet.by = 'supp')

ggline(df, x = 'dose', y = 'len', linetype = 'solid')
ggline(df, x = 'dose', y = 'len', linetype = 'dashed')
ggline(df, x = 'dose', y = 'len', linetype = 'dotted')

ggline(df, x = 'dose', y = 'len', plot_type = 'b')
ggline(df, x = 'dose', y = 'len', plot_type = 'l')
ggline(df, x = 'dose', y = 'len', plot_type = 'p')

ggline(df, x = 'dose', y = 'len', shape = 19)
ggline(df, x = 'dose', y = 'len', shape = 17)
ggline(df, x = 'dose', y = 'len', shape = 15)
ggline(df, x = 'dose', y = 'len', shape = 13)
ggline(df, x = 'dose', y = 'len', shape = 11)

ggline(df, x = 'dose', y = 'len', point.color = 'red')

ggline(df, x = 'dose', y = 'len', point.size = 2)
ggline(df, x = 'dose', y = 'len', point.size = 1)
ggline(df, x = 'dose', y = 'len', point.size = .5)

ggline(df, x = 'dose', y = 'len', size = 1)
ggline(df, x = 'dose', y = 'len', size = .5)

ggline(df, x = 'dose', y = 'len', add = "none")
ggline(df, x = 'dose', y = 'len', add = "dotplot")
ggline(df, x = 'dose', y = 'len', add = "jitter")
ggline(df, x = 'dose', y = 'len', add = "boxplot")
ggline(df, x = 'dose', y = 'len', add = "point")
ggline(df, x = 'dose', y = 'len', add = "mean")
ggline(df, x = 'dose', y = 'len', add = "mean_se")
ggline(df, x = 'dose', y = 'len', add = "mean_sd")
ggline(df, x = 'dose', y = 'len', add = "mean_ci")
ggline(df, x = 'dose', y = 'len', add = "mean_range")
ggline(df, x = 'dose', y = 'len', add = "median")
ggline(df, x = 'dose', y = 'len', add = "median_iqr")
ggline(df, x = 'dose', y = 'len', add = "median_hilow")
ggline(df, x = 'dose', y = 'len', add = "median_q1q3")
ggline(df, x = 'dose', y = 'len', add = "median_mad")
ggline(df, x = 'dose', y = 'len', add = "median_range")

ggline(df, x = 'dose', y = 'len', ggtheme = theme())
ggline(df, x = 'dose', y = 'len', ggtheme = theme_bw())
ggline(df, x = 'dose', y = 'len', ggtheme = theme_classic())
ggline(df, x = 'dose', y = 'len', ggtheme = theme_classic2())
ggline(df, x = 'dose', y = 'len', ggtheme = theme_cleveland())
ggline(df, x = 'dose', y = 'len', ggtheme = theme_get())
ggline(df, x = 'dose', y = 'len', ggtheme = theme_dark())
ggline(df, x = 'dose', y = 'len', ggtheme = theme_gray())
ggline(df, x = 'dose', y = 'len', ggtheme = theme_light())
ggline(df, x = 'dose', y = 'len', ggtheme = theme_linedraw())
ggline(df, x = 'dose', y = 'len', ggtheme = theme_minimal())
ggline(df, x = 'dose', y = 'len', ggtheme = theme_pubclean())
ggline(df, x = 'dose', y = 'len', ggtheme = theme_pubr())
ggline(df, x = 'dose', y = 'len', ggtheme = theme_replace())
ggline(df, x = 'dose', y = 'len', ggtheme = theme_set())
ggline(df, x = 'dose', y = 'len', ggtheme = theme_test())
ggline(df, x = 'dose', y = 'len', ggtheme = theme_transparent())
ggline(df, x = 'dose', y = 'len', ggtheme = theme_update())
ggline(df, x = 'dose', y = 'len', ggtheme = theme_void())

ggline(df, x = 'dose', y = 'len', select = '0.5')
ggline(df, x = 'dose', y = 'len', select = c('1', '2'))
ggline(df, x = 'dose', y = 'len', select = c('0.5', '1'))

ggline(df, x = 'dose', y = 'len', remove = '0.5')
ggline(df, x = 'dose', y = 'len', remove = c('1', '2'))
ggline(df, x = 'dose', y = 'len', remove = c('0.5', '2'))

ggline(df, x = 'dose', y = 'len', order = c("0.5", "1", "2"))
ggline(df, x = 'dose', y = 'len', order = c("1", "2", "0.5"))

ggline(df, x = 'dose', y = 'len', color = 'supp')
ggline(df, x = 'dose', y = 'len', color = 'dose')

ggline(df, x = 'dose', y = 'len', xlab = F, ylab = F)
ggline(df, x = 'dose', y = 'len', xlab = 'Group', ylab = 'Length(g)')


ggline(df, x = "dose", y = "len", 
       color = "supp", add = c("mean_se", "point"), palette = c("#00AFBB", "#E7B800"))


dev.off()

boxplot(len ~ supp * dose, data = df)
boxplot(len ~ supp * dose, data = df, notch = TRUE)
boxplot(len ~ supp * dose, data = df, boxwex = 0.5)
boxplot(len ~ supp * dose, data = df, add = TRUE)
boxplot(len ~ supp * dose, data = df, main = '- Main Title -')
boxplot(len ~ supp * dose, data = df, subset = dose == '1')
boxplot(len ~ supp * dose, data = df, horizontal = T)
boxplot(len ~ supp * dose, data = df, frame = FALSE)
boxplot(len ~ supp * dose, data = df, col = c("#00AFBB", "#E7B800", "#FC4E07"))
boxplot(len ~ supp * dose, data = df, xlab = 'Treatment', ylab = 'Weight')

boxplot(len ~ supp * dose, data = df,
        ylab = "Tooth Length", xlab = "supp * dose",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))


dev.off()

library(gplots)

plotmeans(len ~ dose, data = df)

plotmeans(len ~ dose, data = df, connect = F)
plotmeans(len ~ dose, data = df, connect = T)

plotmeans(len ~ dose, data = df, ccol = 'green')
plotmeans(len ~ dose, data = df, barcol = 'red')
plotmeans(len ~ dose, data = df, col = 'red')

plotmeans(len ~ dose, data = df, barwidth = 1)
plotmeans(len ~ dose, data = df, barwidth = 2)
plotmeans(len ~ dose, data = df, barwidth = .5)

plotmeans(len ~ dose, data = df, text.n.label='size=')
plotmeans(len ~ dose, data = df, text.n.label='n=')

plotmeans(len ~ dose, data = df, n.label = F)
plotmeans(len ~ dose, data = df, n.label = T)

plotmeans(len ~ dose, data = df, ci.label=T)
plotmeans(len ~ dose, data = df, ci.label=F)

plotmeans(len ~ dose, data = df, p = .95)
plotmeans(len ~ dose, data = df, p = .99)

plotmeans(len ~ dose, data = df, bars = F)
plotmeans(len ~ dose, data = df, bars = T)

plotmeans(len ~ dose, data = df, subset = supp == 'VC')

plotmeans(len ~ dose, data = df, frame = F)
plotmeans(len ~ dose, data = df, frame = T)

plotmeans(len ~ dose, data = df, frame = F, xlab = 'dose', y = 'Tooth Length')

plotmeans(len ~ dose, data = df, frame = FALSE,
          xlab = "dose", ylab = "Tooth Length", p = .95,
          main="Mean Plot with 95% CI") 


dev.off()

?interaction.plot

interaction.plot(x.factor = df$dose, trace.factor = df$supp, response = df$len, fun = mean)
interaction.plot(x.factor = df$dose, trace.factor = df$supp, response = df$len, fun = median)
interaction.plot(x.factor = df$dose, trace.factor = df$supp, response = df$len, fun = sd)

interaction.plot(x.factor = df$dose, trace.factor = df$supp, response = df$len, fun = mean, fixed = T)
interaction.plot(x.factor = df$dose, trace.factor = df$supp, response = df$len, fun = mean, fixed = F)

interaction.plot(x.factor = df$dose, trace.factor = df$supp, response = df$len, fun = mean, legend = F)
interaction.plot(x.factor = df$dose, trace.factor = df$supp, response = df$len, fun = mean, legend = T)

interaction.plot(x.factor = df$dose, trace.factor = df$supp, response = df$len, fun = mean, type = 'l')
interaction.plot(x.factor = df$dose, trace.factor = df$supp, response = df$len, fun = mean, type = 'p')
interaction.plot(x.factor = df$dose, trace.factor = df$supp, response = df$len, fun = mean, type = 'b')
interaction.plot(x.factor = df$dose, trace.factor = df$supp, response = df$len, fun = mean, type = 'o')
interaction.plot(x.factor = df$dose, trace.factor = df$supp, response = df$len, fun = mean, type = 'c')

interaction.plot(x.factor = df$dose, trace.factor = df$supp, response = df$len, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Dose", ylab="Tooth Length",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))



#--------------------------------------------------------#
# 4. Compute two-way ANOVA test
#--------------------------------------------------------#

( fit.aov2 <- aov(len ~ supp + dose, data = df) )

class(fit.aov2)
mode(fit.aov2)
names(fit.aov2)

fit.aov2$coefficients
fit.aov2$residuals
fit.aov2$effects
fit.aov2$rank
fit.aov2$fitted.values
fit.aov2$assign
fit.aov2$qr
fit.aov2$df.residual
fit.aov2$contrasts
fit.aov2$xlevels
fit.aov2$call
fit.aov2$terms
fit.aov2$model
fit.aov2$model == df[-4]

summary(fit.aov2)


( fit.aov3 <- aov(len ~ supp * dose, data = df) )
summary(fit.aov3)

( fit.aov3 <- aov(len ~ supp + dose + supp:dose, data = df) )
summary(fit.aov3)



#--------------------------------------------------------#
# 5. Compute some summary statistics
#--------------------------------------------------------#

library(dplyr)

group_by(df, supp, dose) %>% 
  summarise(
    count = n(),
    mean = mean(len, na.rm = T),
    sd = sd(len, na.rm = T)
  )


str(df)

?model.tables

fit.aov3

model.tables(fit.aov3, type = 'means', se = T)
model.tables(fit.aov3, type = 'effects', se = T)



#--------------------------------------------------------#
# 6. Multiple pairwise-comparison between the means of groups
#--------------------------------------------------------#

# 6-1. Tukey multiple pairwise-comparisons - method1

TukeyHSD(fit.aov3)
TukeyHSD(fit.aov3, which = 'dose')


# 6-2. Multiple comparisons using multcomp package - method2

library(multcomp)

summary( glht(fit.aov2, linfct = mcp(dose = 'Tukey')) )
summary( glht(fit.aov3, linfct = mcp(dose = 'Tukey')) )


# 6-3. Pairwise t-test - method3

pairwise.t.test(df$len, df$dose, p.adjust.method = "BH")
pairwise.t.test(df$len, df$supp, p.adjust.method = "BH")



#--------------------------------------------------------#
# 6. Check ANOVA assumptions
#--------------------------------------------------------#

# 6-1. Check the homogeneity of variance assumption - method1

dev.off()
op = par(mfrow=c(2,2))

plot(fit.aov3)

par(op)


plot(fit.aov3, 1)


dev.off()
op = par(mfrow=c(2,3))

plot(fit.aov3, 1)
plot(fit.aov3, 2)
plot(fit.aov3, 3)
plot(fit.aov3, 4)
plot(fit.aov3, 5)
plot(fit.aov3, 6)

par(op)


# 6-2. Check the homogeneity of variance assumption - method2

library(car)
leveneTest(len ~ supp * dose, data = df)


# 6-3. Check the homogeneity of variance assumption - method3
bartlett.test(len ~ supp, data = df)
bartlett.test(len ~ dose, data = df)


# 6-4. ANOVA test with no assumption of equal variances - method4
#      (Welch one-way test)
oneway.test(len ~ supp, data = df)
oneway.test(len ~ dose, data = df)
oneway.test(len ~ supp:dose, data = df)

oneway.test(len ~ supp * dose, data = df)


# 6-5. Pairwise t-tests with no assumption of equal variances - method2
pairwise.t.test(df$len, df$supp, p.adjust.method = "BH", pool.sd = FALSE)
pairwise.t.test(df$len, df$dose, p.adjust.method = "BH", pool.sd = FALSE)

pairwise.t.test(df$len, df$dose:df$supp, p.adjust.method = "BH", pool.sd = FALSE)
pairwise.t.test(df$len, df$supp:df$dose, p.adjust.method = "BH", pool.sd = FALSE)


# 6-6. Check the normality assumption - method1
dev.off()

plot(fit.aov3, 2)


# 6-7. Check the normality assumption - method2
( aov_residuals <- residuals(object = fit.aov3) )

shapiro.test(aov_residuals)






