#--------------------------------------------------------#
# 1. Comparing one-sample mean to a standard known mean
#--------------------------------------------------------#
# One-sample T-test (parametric)
#--------------------------------------------------------#



#--------------------------------------------------------#
# 1. Import your data into R
#--------------------------------------------------------#

set.seed(1234)

rep('M_', 10)

paste('M_', 1:10)
paste0('M_', 1:10)

rnorm(n = 10, mean = 20, sd = 2)
round( rnorm(n = 10, mean = 20, sd = 2) )

df <- data.frame(
    name = paste0(rep('M_', 10), 1:10),
    weight = round(rnorm(n = 10, mean = 20, sd = 20))
)
df

rownames(df)
row.names(df)
colnames(df)
row(df)
col(df)

head(df, 10)
summary(df$weight)



#--------------------------------------------------------#
# 2. Visualize your data using box plots
#--------------------------------------------------------#

library(ggpubr)

?ggboxplot
ggboxplot(data = df$weight)

ggboxplot(data = df$weight, width = 0.5)
ggboxplot(data = df$weight, width = 0.2)
ggboxplot(data = df$weight, width = 0.7)

ggboxplot(data = df$weight, notch = FALSE)
ggboxplot(data = df$weight, notch = TRUE)

ggboxplot(data = df$weight, ylab = 'Weight(g)')

ggboxplot(data = df$weight, xlab = FALSE)
ggboxplot(data = df$weight, xlab = FALSE, ylab = FALSE)

ggboxplot(data = df$weight, ggtheme = theme())
ggboxplot(data = df$weight, ggtheme = theme_bw())
ggboxplot(data = df$weight, ggtheme = theme_classic())
ggboxplot(data = df$weight, ggtheme = theme_classic2())
ggboxplot(data = df$weight, ggtheme = theme_cleveland())
ggboxplot(data = df$weight, ggtheme = theme_get())
ggboxplot(data = df$weight, ggtheme = theme_dark())
ggboxplot(data = df$weight, ggtheme = theme_gray())
ggboxplot(data = df$weight, ggtheme = theme_light())
ggboxplot(data = df$weight, ggtheme = theme_linedraw())
ggboxplot(data = df$weight, ggtheme = theme_minimal())
ggboxplot(data = df$weight, ggtheme = theme_pubclean())
ggboxplot(data = df$weight, ggtheme = theme_pubr())
ggboxplot(data = df$weight, ggtheme = theme_replace())
ggboxplot(data = df$weight, ggtheme = theme_set())
ggboxplot(data = df$weight, ggtheme = theme_test())
ggboxplot(data = df$weight, ggtheme = theme_transparent())
ggboxplot(data = df$weight, ggtheme = theme_update())
ggboxplot(data = df$weight, ggtheme = theme_void())

ggboxplot(data = df$weight, add = "none")
ggboxplot(data = df$weight, add = "dotplot")
ggboxplot(data = df$weight, add = "jitter")
ggboxplot(data = df$weight, add = "boxplot")
ggboxplot(data = df$weight, add = "point")
ggboxplot(data = df$weight, add = "mean")
ggboxplot(data = df$weight, add = "mean_se")
ggboxplot(data = df$weight, add = "mean_sd")
ggboxplot(data = df$weight, add = "mean_ci")
ggboxplot(data = df$weight, add = "mean_range")
ggboxplot(data = df$weight, add = "median")
ggboxplot(data = df$weight, add = "median_iqr")
ggboxplot(data = df$weight, add = "median_hilow")
ggboxplot(data = df$weight, add = "median_q1q3")
ggboxplot(data = df$weight, add = "median_mad")
ggboxplot(data = df$weight, add = "median_range")


ggboxplot(data = df$weight, ylab = 'Weight(g)', xlab = 'distribution')



#--------------------------------------------------------#
# 3. Preleminary test to check one-sample t-test assumptions
#--------------------------------------------------------#

shapiro.test(df$weight)


library(ggpubr)

ggqqplot(data = df$weight, ylab = 'Weight(g)', ggtheme = theme_minimal())



#--------------------------------------------------------#
# 4. Compute one-sample t-test
#--------------------------------------------------------#

res <- t.test(x = df$weight, mu = 25)
res <- t.test(x = df$weight, mu = 25, alternative = 'two.sided')
res <- t.test(x = df$weight, mu = 25, alternative = 'less')
res <- t.test(df$weight, mu = 25, alternative = 'greater')
class(res)
mode(res)
names(res)

res
res$statistic
res$parameter
res$p.value
res$conf.int
res$estimate
res$null.value
res$stderr
res$alternative
res$method
res$data.name
