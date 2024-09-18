#--------------------------------------------------------#
# 1. Comparing Variances (F-test)
#--------------------------------------------------------#
# Compare Multiple Sample Variances
#--------------------------------------------------------#


#--------------------------------------------------------#
# 1. Import and check your data
#--------------------------------------------------------#

?PlantGrowth

data("PlantGrowth")

dim(PlantGrowth)
str(PlantGrowth)
head(PlantGrowth)
View(PlantGrowth, title = 'Plant Growth')
table(PlantGrowth$group)

dplyr::sample_n(PlantGrowth, 10)


?ToothGrowth

data("ToothGrowth")

dim(ToothGrowth)
str(ToothGrowth)
head(ToothGrowth)
View(ToothGrowth, title = 'Tooth Growth')
table(ToothGrowth$supp, ToothGrowth$dose)

ToothGrowth$dose <- as.factor(ToothGrowth$dose)
str(ToothGrowth)



#--------------------------------------------------------#
# 2. Compute Bartlett’s test
#--------------------------------------------------------#

# 2-1. With one independent variable

( fit.bartlett <- bartlett.test(weight ~ group, data = PlantGrowth) )


# 2-2. With multiple independent variables

?interaction

with(ToothGrowth, interaction(supp, dose))

( fit.bartlett2 <- bartlett.test(len ~ interaction(supp, dose), data = ToothGrowth) )



#--------------------------------------------------------#
# 3. Compute Levene’s test
#--------------------------------------------------------#

library(car)

# 3-1. With one independent variable

( fit.levene <- leveneTest(weight ~ group, data = PlantGrowth) )


# 3-2. With multiple independent variables

( fit.levene2 <- leveneTest(len ~ supp * dose, data = ToothGrowth) )



#--------------------------------------------------------#
# 4. Compute Fligner-Killeen test
#--------------------------------------------------------#

( fit.fligner <- fligner.test(weight ~ group, data = PlantGrowth) )

