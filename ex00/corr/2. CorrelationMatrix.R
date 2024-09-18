#----------------------------------------------------------------------------------------#
# Correlation Matrix: To investigate the dependence between multiple variables 
#                     at the same time.
#----------------------------------------------------------------------------------------#
# There are different methods for correlation analysis:
#    (1) Parametric     : Pearson Parametric Correlation Test                 (default)
#    (2) Non-parametric : Spearman & Kendall rank-based Correlation Analysis
#----------------------------------------------------------------------------------------#
# {stats::cor} in R : cor( x, method = c('pearson','kendall','spearman') )
#  Where,
#         x : a numeric vector, matrix or data frame.
#         method : a character string indicating which correlation coefficient 
#                  (or covariance) is to be computed. 
#                  One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
#         na.rm : logical. Should missing values be removed?
#         use	  : an optional character string giving a method for computing covariances
#                 in the presence of missing values. 
#                 This must be (an abbreviation of) one of the strings:
#
#                 "everything",
#                 "all.obs", 
#                 "complete.obs",           - Case-wise deletion
#                 "na.or.complete", 
#                 "pairwise.complete.obs".  - Pair-wise deletion
#
#----------------------------------------------------------------------------------------#
# install.packages('corrplot', dependencies = T)
# install.packages('Hmisc', dependencies = T)
# install.packages('PerformanceAnalytics', dependencies = T)

library(corrplot)
library(Hmisc)
library(PerformanceAnalytics)
library(RColorBrewer)
library(dplyr)


#----------------------------------------------------------------------------------------
# 1. To prepare the data {datasets::mtcars}
#----------------------------------------------------------------------------------------
data("mtcars")

mydata <- mtcars %>% select( c(1,3,4,5,6,7) )

str(mydata)
head(mydata, 6)


#----------------------------------------------------------------------------------------
# 2. To compute correlation matrix of the data
#----------------------------------------------------------------------------------------
# Note: if data contain NA, set "use" option to case-wise deletion("complete.obs")

# ccMatrix <- cor(mtcars, method = 'pearson')     # default method: pearson
# ccMatrix <- cor(mtcars, method = 'pearson', use = "complete.obs") # if data contain NA

ccMatrix <- cor(mtcars)

ccMatrix
round(ccMatrix, 2)


#----------------------------------------------------------------------------------------
# 3. To compute correlation matrix of the data with significance levels (p-value)
#----------------------------------------------------------------------------------------
# By using R function: {Hmisc::rcorr}
# 
#    To compute the significance levels for "Pearson", "Spearman", "Kendall" correlations.
#    This returns both (1) the correlation coefficients and (2) the p-value of 
#    the correlation for all possible pairs of columns in the data table.
#
# Usage: rcorr(x, y, type=c("pearson","spearman"))
#
# Where,
#     x	- A numeric matrix with at least 5 rows and at least 2 columns (if y is absent). 
#         For print, x is an object produced by rcorr.
#     y - A numeric vector or matrix which will be concatenated to x. 
#         If y is omitted for rcorr, x must be a matrix.
#     type - specifies the type of correlations to compute. 
#           Spearman correlations are the Pearson linear correlations computed 
#           on the ranks of non-missing elements, using midranks for ties.
#
# Return,
#     {rcorr} returns :
#          (1) r : a list with elements, the matrix of correlations.
#          (2) n : the matrix of number of observations used in analyzing each pair of variables.
#          (3) P : the asymptotic P-values. 
#
#      Pairs with fewer than 2 non-missing values have the r values set to NA. 
#      The diagonals of n are the number of non-NAs for the single variable 
#      corresponding to that row and column.

# ccMatrixWithPvalue <- rcorr(as.matrix(mydata), type = "spearman")   # Non-Parametric rank-based correlations

# ccMatrixWithPvalue <- rcorr(as.matrix(mydata), type = "pearson")    # Parametric linear correlations
ccMatrixWithPvalue <- rcorr(as.matrix(mydata))                      # default: Parametric Pearson correlations

ccMatrixWithPvalue

ccMatrixWithPvalue[0]
str(ccMatrixWithPvalue)
names(ccMatrixWithPvalue)

ccMatrixWithPvalue$r
ccMatrixWithPvalue$n
ccMatrixWithPvalue$P


#----------------------------------------------------------------------------------------
# 4. To convert returned list into a table (data.frame) with columns: row, col, cor, pval
#----------------------------------------------------------------------------------------
r <- ccMatrixWithPvalue$r
P <- ccMatrixWithPvalue$P

ut <- upper.tri(r)

df_ut <- data.frame(
  row = rownames(r)[row(r)[ut]],
  col = rownames(r)[col(r)[ut]],
  cor = r[ut],
  pvalue = P[ut]
)


#----------------------------------------------------------------------------------------#
# 5. To visualize correlation matrix.
#----------------------------------------------------------------------------------------#
# By using the following methods:
#
#     (1) {stats::symnum} function
#     (2) {corrplot::corrplot} function to plot a "Correlogram"
#     (3) Scatter Plots
#     (4) Heatmap
#----------------------------------------------------------------------------------------#

#----------------------------------------------------------------------------------------
# (1) {stats::symnum} function
#----------------------------------------------------------------------------------------
symnum(ccMatrixWithPvalue$r, abbr.colnames = F)

#----------------------------------------------------------------------------------------
# (2) {corrplot::corrplot} function to plot a "Correlogram"
#----------------------------------------------------------------------------------------
# * Correlogram: 
#        A graph of correlation matrix very useful to highlight 
#        the most correlated variables in a data table.
#
# * Correlation Coefficients: 
#        In this plot, this is colored according to the value.
# 
# * Correation Matrix:
#        This can be also reordered according to the degree of association between variables.
#        **Note) This is important to identify the "hidden structure" and "pattern"
#                in the matrix.
#        
#----------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------
# Method parameter: it supports seven methods, named 
#     (1) "circle" (default) :
#          - Color intensity and the size of the circle are proportional 
#            to the correlation coefficients.
#     (2) "square"
#     (3) "ellipse"
#     (4) "number"
#     (5) "pie"
#     (6) "shade"
#     (7) "color" :
#          - Positive correlations are displayed in blue.
#          - Negative correlations are displayed in red.

corrplot(ccMatrix, method = "circle")   # default
corrplot(ccMatrix, method = "square") 
corrplot(ccMatrix, method = "ellipse") 
corrplot(ccMatrix, method = "number") 
corrplot(ccMatrix, method = "pie") 
corrplot(ccMatrix, method = "shade") 
corrplot(ccMatrix, method = "color") 


corrplot(
  r,                     # To set correlation matrix
  type = "upper",        # To use a upper triangle: "upper", "lower", "full"
  order = "hclust",      # To reorder according to the correlatin coefficient
                         # "hclust": For "hierarchical clustering order" is used
  tl.col = "blue",       # To set color of the text label
  tl.srt = 45,           # To rotate the text label string
  
  method = "color"       # To set the shape of each cell.
)


corrplot(
  r,                     # To set correlation matrix
  type = "upper",        # To use a upper triangle: "upper", "lower", "full"
  order = "hclust",      # To reorder according to the correlatin coefficient
                         # "hclust": For "hierarchical clustering order" is used
  tl.col = "blue",       # To set color of the text label
  tl.srt = 45,           # To rotate the text label string
  
  method = "color",      # To set the shape of each cell.
  
  bg = "lightblue"       # To set background color
)


#----------------------------------------------------------------------------------------
# Caution) {grDevices::colorRampPalette} function returns a function.
#----------------------------------------------------------------------------------------
pal <- colorRampPalette(c('red', 'white', 'blue'))(20)   # function call.

corrplot(
  r,                     # To set correlation matrix.
  type = "upper",        # To use a upper triangle: "upper", "lower", "full".
  order = "hclust",      # To reorder according to the correlatin coefficient.
                         # "hclust": For "hierarchical clustering order" is used.
  tl.col = "blue",       # To set color of the text label.
  tl.srt = 45,           # To rotate the text label string.
  
  method = "color",      # To set the shape of each cell.
  
  col = pal,             # To set color of the circle.
  bg = "lightblue"       # To set background color.
)


#----------------------------------------------------------------------------------------
# To change the color of the correlogram with {RColorBrewer} palette
# by using {RColorBrewer::brewer.pal} function.
#----------------------------------------------------------------------------------------
corrplot(
  r,                     # To set correlation matrix.
  type = "upper",        # To use a upper triangle: "upper", "lower", "full".
  order = "hclust",      # To reorder according to the correlatin coefficient.
                         # "hclust": For "hierarchical clustering order" is used.
  tl.col = "blue",       # To set color of the text label.
  tl.srt = 45,           # To rotate the text label string.
  
  # To set color palette of the circle.
  # col = brewer.pal(n = 8, name = "RdBu"),
  # col = brewer.pal(n = 8, name = "RdYlBu"),
  col = brewer.pal(n = 8, name = "PuOr"),
  
  bg = "lightgray",      # To set background color.
  
  method = "color"       # To set the shape of each cell.
)


#----------------------------------------------------------------------------------------
# To change the color and the roation of text labels in the correlogram.
#----------------------------------------------------------------------------------------
#     parameters, "tl.col" and "tl.srt" are used to change text colors and rotations.
#       - tl.col: For text label color
#       - tl.srt: For text label string rotation
#----------------------------------------------------------------------------------------
corrplot(
  r,                     # To set correlation matrix.
  type = "upper",        # To use a upper triangle: "upper", "lower", "full".
  order = "hclust",      # To reorder according to the correlatin coefficient.
                         # "hclust": For "hierarchical clustering order" is used.
  
  tl.col = "blue",       # To set color of the text label.
  tl.srt = 45,           # To rotate the text label string.
  
  # To set color palette of the circle.
  # col = brewer.pal(n = 8, name = "RdBu"),
  # col = brewer.pal(n = 8, name = "RdYlBu"),
  col = brewer.pal(n = 8, name = "PuOr"),
  
  bg = "lightgray",      # To set background color.
  
  method = "color"       # To set the shape of each cell.
)



#----------------------------------------------------------------------------------------
# Correlogram with the significance level test (P-value)
#----------------------------------------------------------------------------------------
corrplot(
  r,
  type = "upper",
  order = "hclust",
  
  p.mat = P,             # P-value matrix.
  sig.level = 0.01,      # To specify the significance level. 
  
  method = "color"       # To set the shape of each cell. 
)


corrplot(
  r,
  type = "upper",
  order = "hclust",
  
  p.mat = P,             # P-value matrix.
  sig.level = 0.01,      # To specify the significance level.
  
  # Should be one of “pch”, “p-value”, “blank”, “n”, “label_sig”.
  insig = "blank",       # Insignificant correlations leaved blank.      
  
  method = "color"       # To set the shape of each cell.
)


corrplot(
  r,
  type = "upper",
  order = "hclust",
  
  p.mat = P,             # P-value matrix.
  sig.level = 0.01,      # To specify the significance level.
  
  addCoef.col = "white", # add correlation coefficients to each circle.
  
  # Should be one of “pch”, “p-value”, “blank”, “n”, “label_sig”.
  insig = "blank",       # Insignificant correlations leaved blank.  
  
  method = "color"       # To set the shape of each cell.    
)


corrplot(
  r,
  type = "upper",
  order = "hclust",
  
  p.mat = P,             # P-value matrix.
  sig.level = 0.01,      # To specify the significance level.
  
  addCoef.col = "white", # to add correlation coefficients to each circle.
  diag = F,              # To hide correlation coefficient on the principal diagonal.
  
  # Should be one of “pch”, “p-value”, “blank”, “n”, “label_sig”.
  insig = "blank",       # Insignificant correlations leaved blank.
  
  method = "color"       # To set the shape of each cell.
)


#----------------------------------------------------------------------------------------
# (3) Scatter Plots 
#     by using {PerformanceAnalytics::chart.Correlation} : To draw scatter plots
#----------------------------------------------------------------------------------------
# 1. The distribution of each variable is shown on the diagonal.
# 2. On the bottom of the diagonal: 
#         The bivariate scatter plots with a "fitted line" are displayed.
# 3. On the top of the diagonal:
#         The value of the correlation + The significance level as stars.
# 4. Each significance level is associated to a symbol:
#         P-values (0, .001, .01, .05, .1, 1) <=> symbols("***", "**", "*", ".", " ")
#----------------------------------------------------------------------------------------

chart.Correlation(mydata, histogram = T, pch = 19)

#----------------------------------------------------------------------------------------
# (4) Heatmap
#----------------------------------------------------------------------------------------
#     by using {grDevices::colorRampPalette} and {stats::heatmap} functions
#----------------------------------------------------------------------------------------

# function {grDevices::colorRampPalette} returns a function
pal <- colorRampPalette(c("blue", "white", "red"))(20)      # function call.

# x: the correlation matrix, col: color palettes, 
# symm: logical indicating if x should be treated symmetrically.
#       can only be true when x is a square matrix.
heatmap(x = r, col = pal, symm = T)
