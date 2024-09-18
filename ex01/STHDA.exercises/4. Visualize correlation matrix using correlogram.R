#--------------------------------------------------------#
# 1. Install R corrplot package
#--------------------------------------------------------#

# install.packages('corrplot', dependencies = T)
library(corrplot)



#--------------------------------------------------------#
# 2. Data for correlation analysis
#--------------------------------------------------------#

?mtcars

head(mtcars)
dim(mtcars)
rownames(mtcars)
colnames(mtcars)
row(mtcars)
col(mtcars)



#--------------------------------------------------------#
# 3. Computing correlation matrix
#--------------------------------------------------------#

M <- cor(mtcars)
class(M)
M
round(M, 2)



#--------------------------------------------------------#
# 4. Correlogram : Visualizing the correlation matrix
#--------------------------------------------------------#

# should be one of “circle”, “square”, “ellipse”, “number”, “shade”, “color”, “pie”

corrplot(M, method = 'circle')
corrplot(M, method = 'square')
corrplot(M, method = 'ellipse')
corrplot(M, method = 'number')
corrplot(M, method = 'shade')
corrplot(M, method = 'color')
corrplot(M, method = 'pie')



#--------------------------------------------------------#
# 5. Types of correlogram layout
#--------------------------------------------------------#

corrplot(M, type = 'upper')
corrplot(M, type = 'lower')
corrplot(M, type = 'full')



#--------------------------------------------------------#
# 6. Reordering the correlation matrix
#--------------------------------------------------------#

corrplot(M, type = 'upper')
corrplot(M, type = 'upper', order = 'hclust')


colors <- colorRampPalette(c('red', 'green', 'blue'))(100)
class(colors)
colors

corrplot(M, type = 'upper', order = 'hclust', col = colors)
corrplot(M, type = 'lower', order = 'hclust', col = c('black', 'white'), bg = 'lightgray')



#--------------------------------------------------------#
# 7. Changing the color of the correlogram
#--------------------------------------------------------#

library(RColorBrewer)

?brewer.pal
# Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd
colors = brewer.pal(n=8, 'Blues')
colors = brewer.pal(n=8, 'BuGn')
colors = brewer.pal(n=8, 'BuPu')
colors = brewer.pal(n=8, 'GnBu')
colors = brewer.pal(n=8, 'Greens')
colors = brewer.pal(n=8, 'Greys')
colors = brewer.pal(n=8, 'Oranges')
colors = brewer.pal(n=8, 'OrRd')
colors = brewer.pal(n=8, 'PuBu')
colors = brewer.pal(n=8, 'PuBuGn')
colors = brewer.pal(n=8, 'PuRd')
colors = brewer.pal(n=8, 'Purples')
colors = brewer.pal(n=8, 'RdPu')
colors = brewer.pal(n=8, 'Reds')
colors = brewer.pal(n=8, 'YlGn')
colors = brewer.pal(n=8, 'YlGnBu')
colors = brewer.pal(n=8, 'YlOrBr')
colors = brewer.pal(n=8, 'YlOrRd')

colors = brewer.pal(n=8, 'Accent')
colors = brewer.pal(n=8, 'Dark2')
colors = brewer.pal(n=12, 'Paired')
colors = brewer.pal(n=9, 'Pastel1')
colors = brewer.pal(n=8, 'Pastel2')
colors = brewer.pal(n=9, 'Set1')
colors = brewer.pal(n=8, 'Set2')
colors = brewer.pal(n=12, 'Set3')

class(colors)
colors

corrplot(M, type = 'upper', order = 'hclust', col = colors)



#--------------------------------------------------------#
# 8. Changing the color and the rotation of text labels
#--------------------------------------------------------#

corrplot(M, type="upper", order="hclust", tl.col="black", tl.srt=45)



#--------------------------------------------------------#
# 9. Combining correlogram with the significance test
#--------------------------------------------------------#

library(Hmisc)


str(mtcars)

M <- rcorr(as.matrix(mtcars))
M



#--------------------------------------------------------#
# 10. Add significance level to the correlogram
#--------------------------------------------------------#

corrplot(M$r, type="upper", order="hclust", p.mat = M$P, sig.level = 0.01)
corrplot(M$r, type="upper", order="hclust", p.mat = M$P, sig.level = 0.01, insig = "blank")



#--------------------------------------------------------#
# 11. Customize the correlogram
#--------------------------------------------------------#

colors <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
colors <- colorRampPalette(brewer.pal(n=8, 'Pastel2'))

colors(100)


corrplot(M$r, method = 'color', col = colors(100), type = 'lower', order = 'hclust', 
         addCoef.col = 'black', tl.srt = 45, p.mat = M$P, sig.level = 0.01)


corrplot(M$r, method = 'color', col = colors(100), type = 'lower', order = 'hclust', 
         addCoef.col = 'black', tl.srt = 45, p.mat = M$P, sig.level = 0.01, insig = 'blank', diag = FALSE)

