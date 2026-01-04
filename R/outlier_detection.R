source("tests/descriptive statistics.R")
source('global.R')

#1: Outlier detection with z score
data$super <- scale(data$super)
quartz()
hist(data$super)
summary(data$super)

lst <- list()
outliers <- which(data$super > 3.29)
for (i in seq_along(outliers)) {
  x <- outliers[i]
  lst <- append(lst, x)
}
print(paste("There are", length(outliers), "outliers in this dataframe at rows:", 
            paste(unlist(lst), collapse = ", ")))
