#### bike sharing Washington DC dataset analysis ####

library(prophet)
library(lubridate)
library(ggplot2)

# Reading in data
dat <- read.csv("C:/Users/Julian/Downloads/datasets_28865_36778_day.csv")
str(dat)

dat$dteday <- ymd(dat$dteday)

# EDA Plot Timeseries

ggplot(dat, aes(dteday, cnt)) + 
  geom_point() +
  ggtitle("Bike Rentals in Wasington DC")

###### Forecasting model #######

# prepare data to pass it to the prophet function
ds <- dat$dteday
y <- dat$cnt
df <- data.frame(ds, y)

m <- prophet()
m <- fit.prophet(m, df)

# Predictions
future <- make_future_dataframe(m, periods = 10)
forecast <- predict(m, future)

# Plot
dyplot.prophet(m, forecast)
prophet_plot_components(m, forecast)

# model performance
dat$pred <- forecast$yhat[1:731]

ggplot(dat, aes(cnt, pred)) +
  geom_point() +
  geom_smooth(col = "red",
              method = "lm") +
  theme_bw()

summary(lm(pred ~ cnt, dat))

##### Improving model performance, adding US holidays ######

# Incorporating US holidays
m2 <- prophet()
m2 <- add_country_holidays(m2, country_name = "US")
m2 <- fit.prophet(m2, df)

# Predictions
future <- make_future_dataframe(m2, periods = 10)
forecast <- predict(m2, future)

# Plot
dyplot.prophet(m2, forecast)
prophet_plot_components(m2, forecast)

# Model performance
dat$pred <- forecast$yhat[1:731]
summary(lm(pred ~ cnt, dat))

##### Improving Model performance, adding temperature ######
df$temp <- dat$temp
m3 <- prophet()
m3 <- add_country_holidays(m3, country_name = "US")


# Temperature is not available right now
# It is possible to include the weatherforecast or something similar
# Here, I am just creating some random temperatures

temp2 <- runif(10, 0.1, 0.3)
future$temp <- append(dat$temp, temp2)


m3 <- add_regressor(m3, "temp")
m3 <- fit.prophet(m3, df)
forecast <- predict(m3, future)

# Plot
dyplot.prophet(m3, forecast)
prophet_plot_components(m3, forecast)

# Model performance
dat$pred <- forecast$yhat[1:731]
summary(lm(pred ~ cnt, dat))

###### Improving Model performance, Adding humidity #######

df$hum <- dat$hum
m4 <- prophet()
m4 <- add_country_holidays(m4, country_name = "US")
m4 <- add_regressor(m4, "temp")

# Adding 10 random numbers for the forecast
hum2 <- runif(10, 0.4, 0.8)
future$hum <- append(dat$hum, hum2)

m4 <- add_regressor(m4, "hum")
m4 <- fit.prophet(m4, df)
forecast <- predict(m4, future)

# Plot
dyplot.prophet(m4, forecast)
prophet_plot_components(m4, forecast)

# Model performance
dat$pred <- forecast$yhat[1:731]
summary(lm(pred ~ cnt, dat))

