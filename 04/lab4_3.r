variant <- 1 * 25 + 15

set.seed(variant)
cat("Variant:", variant, "\n")

redundant <- round(runif(n = 1, min = 1 + 5, max = 25 - 1))
cat("Redundant: ", redundant, "\n")


# Завдання №3
library(boot)

x <- rnorm(100)
y <- variant * x - ((redundant * 40) / variant) * x^2 + rnorm(100)


calculate_loocv <- function(formula, data) {
  model <- glm(formula, data = data)
  cv_result <- cv.glm(data, model, K = nrow(data))
  return(cv_result$delta[1])
}

data <- data.frame(x = x, y = y)

loocv_error_1 <- calculate_loocv(y ~ poly(x, 1), data)
loocv_error_2 <- calculate_loocv(y ~ poly(x, 2), data)
loocv_error_3 <- calculate_loocv(y ~ poly(x, 3), data)
loocv_error_4 <- calculate_loocv(y ~ poly(x, 4), data)

cat("Тестова помилка LOOCV для моделі з x:", loocv_error_1, "\n")
cat("Тестова помилка LOOCV для моделі з x та x^2:", loocv_error_2, "\n")
cat("Тестова помилка LOOCV для моделі з x, x^2 та x^3:", loocv_error_3, "\n")
cat("Тестова помилка LOOCV для моделі з x, x^2, x^3 та x^4:", loocv_error_4, "\n") #nolint

errors <- c(loocv_error_1, loocv_error_2, loocv_error_3, loocv_error_4)
best_model <- which.min(errors)
cat("\nНайменша тестова помилка у моделі", best_model, "\n")
