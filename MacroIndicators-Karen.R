#setwd("")

library(ggplot2)
library(dplyr)
library(lubridate)
library(corrplot)
library(caret)
library(randomForest)

data <- read.csv("final.csv")
data$date <- as.Date(data$date, format = "%m/%d/%y")
data <- na.omit(data)

summary(data)

correlation_matrix <- data %>%  select_if(is.numeric) %>%
  cor(use = "complete.obs")

corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", 
         title = "   Correlation Matrix", mar = c(0, 0, 1, 0))

reg_model <- lm(SP500 ~ Fed.Funds + Un.Rate + CPI + VIX, data = data)
summary(reg_model)

data_ml <- data %>% select(SP500, Fed.Funds, Un.Rate, CPI, VIX, Cons.Sentiment)

train_index <- createDataPartition(data_ml$SP500, p = 0.8, list = FALSE)
train_data <- data_ml[train_index, ]
test_data <- data_ml[-train_index, ]

rf_model <- randomForest(SP500 ~ ., data = train_data, ntree = 50)
test_data$predictions <- predict(rf_model, newdata = test_data)

ggplot(test_data, aes(x = SP500, y = predictions)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Actual vs Predicted S&P 500") +
  xlab("Actual") +
  ylab("Predicted") +
  theme_minimal()

