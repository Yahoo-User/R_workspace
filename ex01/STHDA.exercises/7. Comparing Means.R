#--------------------------------------------------------#
# 1. Comparing the means of two independent groups
#--------------------------------------------------------#
# Unpaired Two-Samples T-test (parametric)
#--------------------------------------------------------#



#--------------------------------------------------------#
# 1. Import your data into R
#--------------------------------------------------------#

women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
men_weight <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4) 

df <- data.frame( 
  group = rep(c("Woman", "Man"), each = 9),
  weight = c(women_weight,  men_weight)
)
df



#--------------------------------------------------------#
# 2. Check your data
#--------------------------------------------------------#

print(df)

rownames(df)
row.names(df)
colnames(df)
row(df)
col(df)

dim(df)
str(df)


library(dplyr)

group_by(df, group) %>% 
  summarise(
    count = n(),
    mean = mean(weight, na.rm = T),
    sd = sd(weight, na.rm = T)
  )



#--------------------------------------------------------#
# 3. Visualize your data using box plots
#--------------------------------------------------------#

library(ggpubr)

?ggboxplot
ggboxplot(data = df, x = 'group', y = 'weight')

ggboxplot(data = df, x = 'group', y = 'weight', width = 0.5)
ggboxplot(data = df, x = 'group', y = 'weight', width = 0.2)
ggboxplot(data = df, x = 'group', y = 'weight', width = 0.7)

ggboxplot(data = df, x = 'group', y = 'weight', notch = T)

ggboxplot(data = df, x = 'group', y = 'weight', ggtheme = theme())
ggboxplot(data = df, x = 'group', y = 'weight', ggtheme = theme_bw())
ggboxplot(data = df, x = 'group', y = 'weight', ggtheme = theme_classic())
ggboxplot(data = df, x = 'group', y = 'weight', ggtheme = theme_classic2())
ggboxplot(data = df, x = 'group', y = 'weight', ggtheme = theme_cleveland())
ggboxplot(data = df, x = 'group', y = 'weight', ggtheme = theme_get())
ggboxplot(data = df, x = 'group', y = 'weight', ggtheme = theme_dark())
ggboxplot(data = df, x = 'group', y = 'weight', ggtheme = theme_gray())
ggboxplot(data = df, x = 'group', y = 'weight', ggtheme = theme_light())
ggboxplot(data = df, x = 'group', y = 'weight', ggtheme = theme_linedraw())
ggboxplot(data = df, x = 'group', y = 'weight', ggtheme = theme_minimal())
ggboxplot(data = df, x = 'group', y = 'weight', ggtheme = theme_pubclean())
ggboxplot(data = df, x = 'group', y = 'weight', ggtheme = theme_pubr())
ggboxplot(data = df, x = 'group', y = 'weight', ggtheme = theme_replace())
ggboxplot(data = df, x = 'group', y = 'weight', ggtheme = theme_set())
ggboxplot(data = df, x = 'group', y = 'weight', ggtheme = theme_test())
ggboxplot(data = df, x = 'group', y = 'weight', ggtheme = theme_transparent())
ggboxplot(data = df, x = 'group', y = 'weight', ggtheme = theme_update())
ggboxplot(data = df, x = 'group', y = 'weight', ggtheme = theme_void())

ggboxplot(data = df, x = 'group', y = 'weight', add = "none")
ggboxplot(data = df, x = 'group', y = 'weight', add = "dotplot")
ggboxplot(data = df, x = 'group', y = 'weight', add = "jitter")
ggboxplot(data = df, x = 'group', y = 'weight', add = "boxplot")
ggboxplot(data = df, x = 'group', y = 'weight', add = "point")
ggboxplot(data = df, x = 'group', y = 'weight', add = "mean")
ggboxplot(data = df, x = 'group', y = 'weight', add = "mean_se")
ggboxplot(data = df, x = 'group', y = 'weight', add = "mean_sd")
ggboxplot(data = df, x = 'group', y = 'weight', add = "mean_ci")
ggboxplot(data = df, x = 'group', y = 'weight', add = "mean_range")
ggboxplot(data = df, x = 'group', y = 'weight', add = "median")
ggboxplot(data = df, x = 'group', y = 'weight', add = "median_iqr")
ggboxplot(data = df, x = 'group', y = 'weight', add = "median_hilow")
ggboxplot(data = df, x = 'group', y = 'weight', add = "median_q1q3")
ggboxplot(data = df, x = 'group', y = 'weight', add = "median_mad")
ggboxplot(data = df, x = 'group', y = 'weight', add = "median_range")

ggboxplot(df, x = 'group', y = 'weight', color = 'group', palette = c("#00AFBB", "#E7B800"), 
          xlab = 'Groups', ylab = 'Weight')



#--------------------------------------------------------#
# 4. Preleminary test to check independent t-test assumptions
#--------------------------------------------------------#

df

with(df, shapiro.test(weight[group == 'Man']))
with(df, shapiro.test(weight[group == 'Woman']))


table(df$group)

res.f_test <- var.test(weight ~ group, data = df)
res.f_test

class(res.f_test)
mode(res.f_test)
names(res.f_test)

res.f_test$statistic
res.f_test$parameter
res.f_test$p.value
res.f_test$conf.int
res.f_test$estimate
res.f_test$null.value
res.f_test$alternative
res.f_test$method
res.f_test$data.name



#--------------------------------------------------------#
# 5. Compute unpaired two-samples t-test
#--------------------------------------------------------#

res <- t.test(women_weight, men_weight, var.equal = T)
res <- t.test(women_weight, men_weight, var.equal = T, alternative = 'two.sided')
res <- t.test(women_weight, men_weight, var.equal = T, alternative = 'less')
res <- t.test(women_weight, men_weight, var.equal = T, alternative = 'greater')
res

res <- t.test(formula = weight ~ group, data = df, var.equal = T)
res <- t.test(formula = weight ~ group, data = df, var.equal = T, alternative = 'two.sided')
res <- t.test(formula = weight ~ group, data = df, var.equal = T, alternative = 'less')
res <- t.test(formula = weight ~ group, data = df, var.equal = T, alternative = 'greater')
res

class(res)
mode(res)
names(res)

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


