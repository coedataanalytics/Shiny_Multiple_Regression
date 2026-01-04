source("tests/descriptive statistics.R")
source('global.R')

# simple linear regression
m1 <- lm(worksamp ~ hogan, data=data)
summary(m1)
confint(model)
# multiple regression
m1 <- lm(worksamp ~ hogan + biodata + verbal + reason + math, data=data)
summary(m1)
confint(model)

# standardized
dat_standardized_subset <- data %>%
  mutate(across(c("super", "worksamp", "reason", "math", "verbal", "biodata", "hogan"), scale))

# simple linear regression
m2 <- lm(worksamp ~ hogan, data=dat_standardized_subset)
summary(m2)
confint(m2, level=.95)
# multiple regression
m2 <- lm(worksamp ~ hogan + biodata + verbal + reason + math, data=dat_standardized_subset)
summary(m2)
confint(m2, level=.95)

