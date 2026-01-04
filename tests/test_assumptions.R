source("tests/descriptive statistics.R")
source('global.R')
#1 Linearity
predictors <- c('hogan','biodata','math','reason','verbal')
outcomes <- c('super', 'super2', 'worksamp')

#a: Scatter plots------------------------------------ 
plots <- map(predictors, function(pred) {
  map(outcomes, function(out) {
    ggplot(data, aes(x = .data[[pred]], y = .data[[out]])) +
      geom_point(alpha = 0.6) +
      labs(x = pred, y = out) +
      theme_minimal()
  })
})

all_plots <- unlist(plots, recursive = FALSE)

view_plot <- function(pred, out) {
  pred_index <- which(predictors == pred)
  out_index <- which(outcomes == out)
  plot_index <- (pred_index - 1) * length(outcomes) + out_index
  print(all_plots[[plot_index]])
}

quartz()
view_plot('biodata', 'worksamp')

#b: Residual Plots------------------------------------ 
m1 <- lm(worksamp ~ hogan, data = data)
residualPlots(m1, terms = ~ 1)

#c: Breusch-Pagan test------------------------------------
m2 <- lm(worksamp ~ hogan, data = data)
bptest(m2)

#c: Homoscedasticity------------------------------------
# Residuals vs fitted values plot/ QQ plot
par(mfrow = c(2, 2))
plot(m2)

#c: Autocorrelation------------------------------------
# Durbin-Watson Test
dwtest(m2)

#c: Normality------------------------------------
shapiro.test(data$super)
shapiro.test(data$worksamp)

#c: Multicollinearity------------------------------------
m3 <- lm(worksamp ~ hogan + biodata + math + reason + verbal, data=data)
# If > 5, then there is potential concern
vif_result <- vif(m3)
for (i in seq_along(vif_result)) {
  x <- vif_result[i]
  y <- names(vif_result)[i]
  print(x)
  if (x >= 5) {
    print(paste(y, "may have multicollinearity because its VIF is equal to", x))
  }
}
# * Create the pass fail indicators
