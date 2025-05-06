variant <- 1 * 25 + 15

set.seed(variant)
cat("Variant:", variant, "\n")

redundant <- round(runif(n = 1, min = 1 + 5, max = 25 - 1))
cat("Redundant: ", redundant, "\n")


# Завдання №1
library(MASS)

boston_data <- MASS::Boston

reduction_percent <- redundant / 100

modified_boston_data <- boston_data[sample(nrow(boston_data), size = round(nrow(boston_data) * (1 - reduction_percent))), ] # nolint

crime_threshold <- mean(modified_boston_data$crim)
modified_boston_data$high_crime <- ifelse(modified_boston_data$crim > crime_threshold, 1, 0) # nolint


test_errors1 <- numeric(4)
test_errors2 <- numeric(4)

for (i in 1:4) {
  set.seed(variant + i)

  train_index1 <- sample(seq_len(nrow(modified_boston_data)), size = 0.5 * nrow(modified_boston_data)) # nolint
  train_data1 <- modified_boston_data[train_index1, ]
  test_data1 <- modified_boston_data[-train_index1, ]

  model1 <- glm(high_crime ~ nox + rad, data = train_data1, family = "binomial")
  predictions1 <- predict(model1, newdata = test_data1, type = "response")
  test_errors1[i] <- mean(ifelse(predictions1 > 0.5, 1, 0) != test_data1$high_crime) # nolint

  train_index2 <- sample(seq_len(nrow(modified_boston_data)), size = 0.5 * nrow(modified_boston_data)) # nolint
  train_data2 <- modified_boston_data[train_index2, ]
  test_data2 <- modified_boston_data[-train_index2, ]

  model2 <- glm(high_crime ~ nox + rad + medv, data = train_data2, family = "binomial") # nolint
  predictions2 <- predict(model2, newdata = test_data2, type = "response")
  test_errors2[i] <- mean(ifelse(predictions2 > 0.5, 1, 0) != test_data2$high_crime) # nolint
}

cat("Результати для моделі з nox та rad:\n")
for (i in 1:4) {
  cat("Ітерація", i, "- Тестова помилка:", test_errors1[i], "\n")
}

cat("\nРезультати для моделі з nox, rad та medv:\n")
for (i in 1:4) {
  cat("Ітерація", i, "- Тестова помилка:", test_errors2[i], "\n")
}