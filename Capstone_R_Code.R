library(dplyr)
library(lubridate)
library(prophet)
library(stringr)
library(ISOweek)
library(ggplot2)
library(forecast)
library(zoo)
library(Metrics)

## DATA INGESTION

data_path = #WRITE WORKING DIRECTORY PATH
file_name = #WRITE THE NAME OF THE CSV
setwd(data_path)
data <- read.csv(file_name)
data <- data[,1:3]

## DATA CLEANING

date_in_week <- function(yrwk){
  w <- paste0(str_split(yrwk, "-")[[1]][1],
              "-W", 
              str_split(yrwk, "-")[[1]][2], 
              "-", 
              7)
  ISOweek2date(w)
}

data$date <- as_date(apply(data, 1, date_in_week))

products <- list()
for (i in (unique(data$ID_material))){
  x <- paste0(i, "_df")
  products[[x]] <- subset(data, ID_material == i)
  products[[x]]$ds <- as_date(products[[x]]$date)
  products[[x]]$y <- as.integer(products[[x]]$Units)
  products[[x]] <- products[[x]][5:6]
}

## TIME SERIES DECOMPOSITION

for (i in (8:13)){
  decomposed_temp <- decompose(ts(products[[i]][2], frequency = 52), type="mult")
  df_temp <- cbind(products[[i]][1], decomposed_temp$x, decomposed_temp$seasonal, decomposed_temp$trend, decomposed_temp$random)
  name_temp <- str_sub(names(products)[i],1,-4)
  name_temp <- paste0(data_path,name_temp,".csv")
  write.csv(df_temp, name_temp, row.names=FALSE)
}

## DATA SPLITTING

training <- products[[1]] %>%
  slice(1:(nrow(.)-12))

testing <- products[[1]] %>%
  slice((nrow(.)-11):nrow(.))

## PRODUCTS MEAN

products_mean = vector("numeric", length = 0)

for (i in (1:14)){
  temp_testing <- products[[i]] %>%
    slice((nrow(.)-7):nrow(.))
  products_mean <- c(products_mean, mean(unlist(temp_testing[2]), na.rm = TRUE))
}

## SARIMA MODEL

a_model <- auto.arima(training[[2]],approximation = F,allowdrift = T,allowmean = T)
sa_model <- auto.arima(temp_ts[,2])
forecast_sarima <- forecast(a_model, bootstrap = T, h = 12)
plot(forecast_a)

a_model_errors = vector("numeric", length = 0)
for (i in (1:14)){
  temp_training <- products[[i]] %>%
    slice(1:(nrow(.)-8))
  temp_testing <- products[[i]] %>%
    slice((nrow(.)-7):nrow(.))
  temp_a_model <- auto.arima(temp_training[[2]],approximation = F,allowdrift = T,allowmean = T)
  temp_forecast_sarima <- forecast(temp_a_model, bootstrap = T, h = 8)
  a_model_errors <- c(a_model_errors, mae(temp_testing[[2]], temp_forecast_sarima$mean))
}

## PROPHET MODEL

m <- prophet(products[[1]])
future <- make_future_dataframe(m, periods = 12, freq = "week")
forecast <- predict(m, future)
plot(m, forecast)

p_model_errors = vector("numeric", length = 0)
for (i in (1:14)){
  temp_training <- products[[i]] %>%
    slice(1:(nrow(.)-8))
  temp_testing <- products[[i]] %>%
    slice((nrow(.)-7):nrow(.))
  temp_prophet_model <- prophet(temp_training)
  temp_future_df <- make_future_dataframe(temp_prophet_model, periods = length(temp_testing[[1]]), freq = "week")
  temp_forecast_prophet <- predict(temp_prophet_model, temp_future_df)
  temp_forecast_prophet <- temp_forecast_prophet %>%
    slice((nrow(.)-8):nrow(.))
  p_model_errors <- c(p_model_errors, mae(temp_testing[[2]], temp_forecast_prophet$yhat))
}