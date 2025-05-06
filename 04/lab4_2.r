variant <- 1 * 25 + 15

set.seed(variant)
cat("Variant:", variant, "\n")

redundant <- round(runif(n = 1, min = 1 + 5, max = 25 - 1))
cat("Redundant: ", redundant, "\n")


# Завдання №2
library(ISLR)
library(boot)

auto_data <- ISLR::Auto

reduction_percent <- redundant / 100
modified_auto_data <- auto_data[sample(nrow(auto_data), size = round(nrow(auto_data) * (1 - reduction_percent))), ] # nolint

cat("\n-------\n")

mpg_mean <- mean(modified_auto_data$mpg)
cat("Оцінка середнього значення mpg:", mpg_mean, "\n")

std_error_mean <- sd(modified_auto_data$mpg) / sqrt(nrow(modified_auto_data)) # nolint
cat("Стандартна похибка середнього (традиційний метод):", std_error_mean, "\n")

cat("\n-------\n")
bootstrap_mean <- function(data, indices) {
  return(mean(data[indices]))
}
set.seed(variant)
boot_mean <- boot(data = modified_auto_data$mpg, statistic = bootstrap_mean, R = 1000) # nolint
cat("Стандартна похибка середнього (бутстрап):", sd(boot_mean$t), "\n")


cat("\n-------\n")
bootstrap_median <- function(data, indices) {
  return(median(data[indices]))
}
set.seed(variant)
boot_median <- boot(data = modified_auto_data$mpg, statistic = bootstrap_median, R = 1000) # nolint
cat("Оцінка медіани mpg:", median(modified_auto_data$mpg), "\n")
cat("Стандартна похибка медіани (бутстрап):", sd(boot_median$t), "\n")

bootstrap_percentile_10 <- function(data, indices) {
  return(quantile(data[indices], probs = 0.1))
}

set.seed(variant)
boot_percentile_10 <- boot(data = modified_auto_data$mpg, statistic = bootstrap_percentile_10, R = 1000) # nolint
cat("Оцінка десятого процентиля mpg:", quantile(modified_auto_data$mpg, probs = 0.1), "\n") # nolint
cat("Стандартна похибка десятого процентиля (бутстрап):", sd(boot_percentile_10$t), "\n") # nolint
