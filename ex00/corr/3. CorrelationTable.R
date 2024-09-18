#----------------------------------------------------------------------------------------#
# Correlation Matrix Analysis: 
#
#   is an import method to find "dependence" between multiple variables.
#   and is very useful to study "dependence" or "associations" between variables.
#----------------------------------------------------------------------------------------#
# {xtable} package:
#
#   To display a nice correlation table in HTML or LaTex formats.
#----------------------------------------------------------------------------------------#

# install.packages('xtable', dependencies = T)

library(xtable)


#----------------------------------------------------------------------------------------#
# 1. To create sample data using {datasets:mtcars}
#----------------------------------------------------------------------------------------#
data(mtcars)

str(mtcars)
head(mtcars, 6)


#----------------------------------------------------------------------------------------#
# 2. To compute the correlation matrix using {stats::cor} function
#----------------------------------------------------------------------------------------#
mcor <- round(cor(mtcars), 2)

mcor[0]
class(mcor)
mcor


#----------------------------------------------------------------------------------------#
# 3. To get the lower/upper part of a correlation matrix
#    by using {base::upper.tri} function and {base::lower.tri} function.
#----------------------------------------------------------------------------------------#
# Parameters:
#     x - The correlation matrix
#     diag - if TRUE, the diagonal are not included in the result.
#
# Returns: A matrix of logicals which has the same size of the correlation matrix.
#          The entries are TRUE in the lower/upper triangle.
#----------------------------------------------------------------------------------------#
# matrixOfPartWithLogical <- lower.tri(x = mcor, diag = F)
matrixOfPartWithLogical <- upper.tri(x = mcor, diag = F)

matrixOfPartWithLogical[0]
class(matrixOfPartWithLogical)
matrixOfPartWithLogical

#----------------------------------------------------------------------------------------
# To "hide" the lower/upper triangle.
#----------------------------------------------------------------------------------------
upper <- mcor
upper[upper.tri(mcor)] <- ""
upper <- as.data.frame(upper)
upper

lower <- mcor
lower[lower.tri(mcor)] <- ""
lower <- as.data.frame(lower)
lower


#----------------------------------------------------------------------------------------#
# 4. To use {xtable} package to display a "nice correlation table" in "HTML" format
#    by using {xtable::xtable} function.
#----------------------------------------------------------------------------------------#
cortbl <- xtable(upper)

class(cortbl)    # "xtable", "data.frame"

print(cortbl, type = "html")

