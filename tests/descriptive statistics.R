source("global.R")

continuous_data <- select(data, -sex, -race, -super, -super2)
nominal_data <- select(data, sex, race)

# Source - https://stackoverflow.com/a/62017887/23243909
continuous_descriptives <- stat.desc(continuous_data, desc = TRUE, norm = FALSE)
continuous_descriptives <- continuous_descriptives[rownames(continuous_descriptives) %in% c("mean", "std.dev", "min", "max", "range"), ] #nolint
# continuous_descriptives <- write.csv(descriptive_statistics, row.names = TRUE)

# Source - https://stackoverflow.com/a/31145812
#missing data count 
na_count <- sapply(data, function(y) sum(length(which(is.na(y)))))
na_count

# frequency counts
nominal_data$race[nominal_data$race == 0] <- "Black"
nominal_data$race[nominal_data$race == 1] <- "White"
nominal_data$sex[nominal_data$sex == 0] <- 'Female'
nominal_data$sex[nominal_data$sex == 1] <- 'Male'

table <- table(nominal_data)
prob_table <- table / sum(table)
print(prob_table)

cor(continuous_data)
