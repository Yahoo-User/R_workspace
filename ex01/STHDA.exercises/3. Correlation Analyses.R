#--------------------------------------------------------#
# 1. Visualize Correlation Matrix using Correlogram
#--------------------------------------------------------#

library(corrplot)
library(RColorBrewer)

?mtcars
mtcars

dim(mtcars)


M <- cor(mtcars)
M <- cor(mtcars, method = "pearson")
M <- cor(mtcars, method = "kendall")
M <- cor(mtcars, method = "spearman")

M <- cor(mtcars, method = "pearson", use = 'complete.obs')
M <- cor(mtcars, method = "pearson", use = 'pairwise.complete.obs')

class(M)
M

?corrplot
?brewer.pal
corrplot(M, type = 'upper', order = 'hclust', col = brewer.pal(n=8, name = 'RdYlBu'))


#--------------------------------------------------------#
# 2. Compute correlation matrix
#--------------------------------------------------------#

data("mtcars")

df <- mtcars[c(1,3,4,5,6,7)]
head(df, 6)

?cor
M <- cor(df)
M <- cor(df, use = 'everything')
M <- cor(df, use = 'complete.obs')
M <- cor(df, use = 'complete.obs', method = 'pearson')
M <- cor(df, use = 'complete.obs', method = 'kendall')
M <- cor(df, use = 'complete.obs', method = 'spearman')

class(M)
M
round(M, 2)


#--------------------------------------------------------#
# 3. Correlation matrix with significance levels (p-value)
#--------------------------------------------------------#

library(Hmisc)

?rcorr
M <- rcorr(as.matrix(df))
M <- rcorr(as.matrix(df), type = 'pearson')
M <- rcorr(as.matrix(df), type = 'spearman')
class(M)
mode(M)
names(M)

M
M$r
M$n
M$P

?upper.tri
?lower.tri

upper.tri(M$r)
upper.tri(M$r, diag = FALSE)
upper.tri(M$P, diag = TRUE)

M$r[ upper.tri(M$r) ] <- NA
M$r

lower.tri(M$r)
lower.tri(M$r, diag = FALSE)
lower.tri(M$r, diag = TRUE)

M$P[ lower.tri(M$P) ] <- NA
M$P

M


#--------------------------------------------------------#
# 4. Flatten Correlation matrix with significance levels (p-value)
#--------------------------------------------------------#

library(Hmisc)

head(mtcars)
head(mtcars[1:7])

M <- rcorr(as.matrix(mtcars[c(1:7)]))
M

?row

M$r
row(M$r)

?col
col(M$r)

ut <- upper.tri(M$r)
ut

row(M$r)[ut]
col(M$r)[ut]

M$r[ut]
M$P[ut]


ut <- upper.tri(M$r)
df_flattencormat <- data.frame(
  row = rownames(M$r)[ row(M$r)[ut] ],
  col = rownames(M$r)[ col(M$r)[ut] ],
  cor = M$r[ut],
  P = M$P[ut]
)

df_flattencormat


#--------------------------------------------------------#
# 5. Visualize correlation matrix
#--------------------------------------------------------#

# 5-1. Use symnum() function: Symbolic number coding
?symnum
M$r

symnum(M$r)
symnum(M$r, abbr.colnames = TRUE)
symnum(M$r, abbr.colnames = FALSE)
symnum(M$r, symbols = c(" ", ".", ",", "+", "*", "B"), abbr.colnames = FALSE)


# 5-2. Use corrplot() function: Draw a correlogram
library(corrplot)
corrplot(M$r)
corrplot(M$r, type = "upper")
corrplot(M$r, type = "lower")
corrplot(M$r, order = 'hclust')
corrplot(M$r, tl.col = 'black')
corrplot(M$r, tl.srt = 45)
corrplot(M$r, tl.srt = 0)
corrplot(M$r, tl.offset = 0)
corrplot(M$r, tl.offset = 2)
corrplot(M$r, tl.offset = 5)
corrplot(M$r, tl.cex = 1)
corrplot(M$r, tl.cex = 2)
corrplot(M$r, tl.cex = .8)
corrplot(M$r, tl.pos = "lt")
corrplot(M$r, tl.pos = "dt")
corrplot(M$r, sig.level = 0.01, p.mat = M$P)
corrplot(M$r, sig.level = 0.01, p.mat = M$P, insig = 'blank')
corrplot(M$r, type = "lower", order = 'hclust')
corrplot(M$r, type = "upper", order = "hclust", tl.col = "blue", tl.srt = 45)


# 5-3. Use chart.Correlation(): Draw scatter plots
library(PerformanceAnalytics)
?chart.Correlation

df <- mtcars[c(1,3,4,5,6,7)]
head(df)

chart.Correlation(df)
chart.Correlation(df, pch = 19)
chart.Correlation(df, pch = 9)
chart.Correlation(df, histogram = TRUE)
chart.Correlation(df, histogram = FALSE)
chart.Correlation(df, histogram = FALSE, pch = 19)
chart.Correlation(df, method = 'pearson')
chart.Correlation(df, method = 'kendall')
chart.Correlation(df, method = 'spearman')


# 5-4. Use heatmap()
colors <- colorRampPalette(c('blue', 'white', 'red'))(20)
colors

?heatmap

M$r

heatmap(M$r)

heatmap(M$r, scale='row')
heatmap(M$r, scale='column')
heatmap(M$r, scale='none')

heatmap(M$r, symm = FALSE)
heatmap(M$r, symm = TRUE)

heatmap(M$r, col = colors)

heatmap(M$r, col = colors, symm = FALSE)
heatmap(M$r, col = colors, symm = TRUE)
