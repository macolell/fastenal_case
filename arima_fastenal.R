#install.packages("TSA")
#install.packages("forecast")
#install.packages("lubridate")
setwd("~/Programming/Data")
library(TSA)
library(dplyr)
library(tseries)
library(forecast)
library(lubridate)

df <- read.csv("cust_total_sales.csv")
q1_20 <- sum(df$q1_sales_2020)
q2_20 <- sum(df$q2_sales_2020)
q3_20 <- sum(df$q3_sales_2020)
q4_20 <- sum(df$q4_sales_2020)
q1_21 <- sum(df$q1_sales_2021)
q2_21 <- sum(df$q2_sales_2021)
q3_21 <- sum(df$q3_sales_2021)
q4_21 <- sum(df$q4_sales_2021)


df2 <- read.csv("arima_2.csv", header = T, stringsAsFactors = F)
total_sales <- c(q1_20, q2_20, q3_20, q4_20, q1_21, q2_21, q3_21, q4_21 )

data <- df2
data$total_sales <- total_sales

curr_df_train <- data[1:(nrow(data)-1),]
curr_df_test <- data[8,]

#write.csv(data,"~/Programming/Data/arima_total_sales.csv", row.names = FALSE)


###ROUND 2 ARIMA
install.packages("forecast")
install.packages("fable")
install.packages("fpp")
library(forecast)
library(ggplot2)
library(fable)
library(fpp)
library(gridExtra)
#library(forecast)
fit_cons <- auto.arima(curr_df_train$total_sales)
fcast_cons <- forecast(fit_cons, h = 3)
autoplot(fcast_cons,alpha = 0.5,
         shape = 1, xlab = "Quarters", ylab = "Total Sales",
         legendLabs = NULL, CI = FALSE, bands = FALSE,
         pval = FALSE, plotTable = FALSE, divideTime = 1,
         returnTable = FALSE)



accuracy(fit_cons)

fit_cons_MANEMP <- auto.arima(curr_df_train$total_sales, xreg = curr_df_train$MANEMP)
fcast_cons_MANEMP <- forecast(fit_cons_MANEMP, xreg = curr_df_test$MANEMP, h = 1)
autoplot(fcast_cons_MANEMP)
forecast(fit_cons_MANEMP, xreg = curr_df_test$MANEMP, h = 1)

MANEMP_col <- matrix(data$MANEMP, ncol = 1)
UNRATE_col <- matrix(data$UNRATE, ncol = 1)
GDPC1_col <- matrix(data$GDPC1, ncol = 1)
CPIAUCSL_col <- matrix(data$CPIAUCSL, ncol = 1)
UMCSENT_col <- matrix(data$UMCSENT, ncol = 1)

####
MANEMP_col_ <- matrix(data$MANEMP, ncol = 1)
UNRATE_col <- matrix(data$UNRATE, ncol = 1)
GDPC1_col <- matrix(data$GDPC1, ncol = 1)
CPIAUCSL_col <- matrix(data$CPIAUCSL, ncol = 1)
UMCSENT_col <- matrix(data$UMCSENT, ncol = 1)

#income <- c(NA, NA, df$income)
#income_matrix <- embed(income, 3)
vars_matrix <- cbind(MANEMP_col, UNRATE_col, GDPC1_col, CPIAUCSL_col, UMCSENT_col)
print(vars_matrix)

fit_vars_0 <- auto.arima(data$total_sales, xreg = vars_matrix[, 1:2])
fit_vars_1 <- auto.arima(data$total_sales, xreg = vars_matrix[, 1:3])
fit_vars_2 <- auto.arima(data$total_sales, xreg = vars_matrix[, 1:4])
fit_vars_3 <- auto.arima(data$total_sales, xreg = vars_matrix[, 1:5])
print(fit_vars_0$aic)
print(fit_vars_1$aic)
print(fit_vars_2$aic)
print(fit_vars_3$aic)

accuracy(fit_cons)
accuracy(fit_vars_3)


fit_vars_4 <- arimax(data$total_sales, xreg = vars_matrix, method = "CSS-ML")

checkresiduals(fit_vars_4)

#LINEAR MODEL
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
  
}

data_norm <- as.data.frame(lapply(data[2:7], min_max_norm))
head(data_norm)

model <- lm(total_sales ~ CPIAUCSL + UMCSENT + MANEMP + GDPC1, data = data_norm)
summary(model)
corr <- cor(data[2:6])
library(corrplot)
corrplot(corr)
