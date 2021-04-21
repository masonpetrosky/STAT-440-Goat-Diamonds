library(data.table)
library(ggplot2)
attach(diamonds)
diamonds <- data.table(diamonds)
# unique(cut)
# unique(color)
# unique(clarity)
model <- lm(price ~ carat + depth + table + x + y + z, data = diamonds)
# summary(model)
train <- diamonds

# Leave-One-Out Cross Validation
set.seed(101)
fitted_value <- NULL
squaredError <- 0
  for(i in 1:nrow(train)){
    validation <- train[i,]
    training <- train[-i,]
    lm <- lm(price ~ carat + depth + table + x + y + z, data = diamonds)
    fitted_value[i] <- predict(lm, validation)
    squaredError <- squaredError + (fitted_value[i] - validation$price)^2
  }
sqrt(squaredError/nrow(train))