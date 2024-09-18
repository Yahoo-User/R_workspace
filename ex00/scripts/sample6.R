detach(name = "package:readxl", unload = TRUE)
library(readxl)

df_exam <- read_excel(path = './data/excel_exam_sheet.xlsx', sheet = 3, col_names = TRUE)
df_exam

dim(df_exam)
head(df_exam)
tail(df_exam)
mode(df_exam)
typeof(df_exam)
df_exam[0]

colnames(df_exam)
rownames(df_exam)
