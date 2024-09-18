
install.packages('readr', dependencies = TRUE)

library(readr)

csv_exam <- read_csv(file = "./data/csv_exam.csv", col_names = TRUE)
View(csv_exam)

csv_exam[0]
typeof(csv_exam)
mode(csv_exam)

head(csv_exam)
tail(csv_exam)
colnames(csv_exam)
rownames(csv_exam)
