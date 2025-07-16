
# R Script: Mental Health Analysis During COVID-19
# Author: Nilanjan Acharjya
# Description: Analysis of Depression, Anxiety, and Stress using survey data

# Load necessary libraries

library(rcompanion)
library(ggplot2)

dep<-read.csv("C:\\Users\\NILANJAN\\OneDrive\\Desktop\\dep.csv",head=TRUE)
dep
table(dep$anxiety)
table(dep$ranx)
table(dep$stress)
table(dep$rstress)
table(dep$depression)
table(dep$rdep)
table(dep$number)
table(dep$MoneyAmount)
table(dep$Education)
table(dep$sex)
# Check missing values
sum(is.na(dep))
which(is.na(dep))

# ---------------------- Pie Charts ----------------------
# Depression levels
slices <- table(dep$rdep)
lbls <- names(slices)
pct <- round(slices / sum(slices) * 100)
lbls <- paste(lbls, pct, "%", sep = "")
pie(slices, labels = lbls, main = "Pie Chart of Depression", col = rainbow(length(lbls)))

# Stress levels
slices <- table(dep$rstress)
lbls <- names(slices)
pct <- round(slices / sum(slices) * 100)
lbls <- paste(lbls, pct, "%", sep = "")
pie(slices, labels = lbls, main = "Pie Chart of Stress", col = rainbow(length(lbls)))

# Anxiety levels
slices <- table(dep$ranx)
lbls <- names(slices)
pct <- round(slices / sum(slices) * 100)
lbls <- paste(lbls, pct, "%", sep = "")
pie(slices, labels = lbls, main = "Pie Chart of Anxiety", col = rainbow(length(lbls)))

# ---------------------- Bar Charts ----------------------
# Mental health by city (Depression)
depression <- table(dep$city, dep$rdep)
barplot(t(depression), beside = TRUE, col = rainbow(5),
        legend = colnames(depression),
        main = "Depression by City", xlab = "City", ylab = "Count")

# ---------------------- Box Plots ----------------------
boxplot(dep$anxiety ~ dep$sex, col = c("red", "green"), main = "Anxiety by Gender")
boxplot(dep$stress ~ dep$sex, col = c("blue", "orange"), main = "Stress by Gender")
boxplot(dep$depression ~ dep$sex, col = c("purple", "yellow"), main = "Depression by Gender")

# ---------------------- Chi-Square Tests ----------------------
# Stress vs. Depression
chisq.test(rbind(table(dep$rdep), table(dep$rstress)))

# Anxiety vs. Stress
chisq.test(rbind(table(dep$ranx), table(dep$rstress)))

# Anxiety vs. Depression
chisq.test(rbind(table(dep$ranx), table(dep$rdep)))

# Anxiety vs. City
chisq.test(table(dep$ranx, dep$city))

# Depression vs. Education
chisq.test(table(dep$Education, dep$rdep))

# ---------------------- Cramér's V ----------------------
mental_data <- matrix(c(138,126,206,664,141,
                        43,102,144,840,146,
                        296,83,140,687,69), nrow = 3, byrow = TRUE)
cramerV(mental_data)

# ---------------------- Two-sample t-tests ----------------------
t.test(anxiety ~ sex, var.equal = TRUE, data = dep)
t.test(stress ~ sex, var.equal = TRUE, data = dep)
t.test(depression ~ sex, var.equal = TRUE, data = dep)

