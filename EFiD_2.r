library(aTSA)
library(fGarch)
library(forecast)
library(rugarch)
library(tseries)
library(urca)
source('EFiD_2_functions.r')

dataset <- read.csv('chfeur_d.csv')
dataset <- cbind.data.frame(dataset$Data, dataset$Zamkniecie)
colnames(dataset) <- c('Date', 'Closing')
rates <- function_rate_of_return(dataset)

# --- stationarity ---
adf.test(rates$Rate_of_Return)
ur.df(rates$Rate_of_Return) 

# --- ARMA / ARIMA ---
model_auto <- auto.arima(rates$Rate_of_Return)
summary(model_auto)
model_arima <- arima(rates$Rate_of_Return, order = c(1,0,2))
summary(model_arima)

# --- autocorelation ---
Box_result <- c()
for(i in 1:30){
    Box_result[i] <- Box.test(model_arima$residuals, i)$p.value
}

# --- ARCH ---
arch.test(model_arima)

# ------------ GARCH ------------------
# options: “norm”, “snorm”, “ged”, “sged”, “std”, “sstd”, “snig”, “QMLE”
garch_sstd <- garchFit(~garch(1,1), data = model_arima$residuals, trace = FALSE, cond.dist = 'sstd')
garch_norm <- garchFit(~garch(1,1), data = model_arima$residuals, trace = FALSE, cond.dist = 'norm')
summary(garch_norm)
summary(garch_sstd)

# standardized residuals = fit3@residuals/fit3@sigma.t ; nu = shape; xi = skew
ks.test(garch_sstd@residuals/garch_sstd@sigma.t, 'psstd', nu=7.351, xi=0.9562)

forecast_GARCH <- as.data.frame(fGarch::predict(garch_sstd, n.ahead = 5, plot=T))
forecast_GARCH

# ------------ Forecast -------------------
# forecast: rates of return (logarithmic)
forecast_arima_RoR <- as.data.frame(forecast::forecast(model_arima, h=5, level = 0.95))
sd_times_cv_RoR <- forecast_GARCH[,1] - forecast_GARCH[,4]
forecast_arima_RoR[,4] <- forecast_arima_RoR[,1] - sd_times_cv_RoR
forecast_arima_RoR[,5] <- forecast_arima_RoR[,1] + sd_times_cv_RoR
forecast_arima_RoR

# GARCH model based on residuals from ARIMA
garch_sstd_arima <- garchFit(~garch(1,1), data = arima(log(dataset[,2]), order = c(1,1,2))$residuals, trace = FALSE, cond.dist = 'sstd')
summary(garch_sstd_arima)
ks.test(garch_sstd_arima@residuals/garch_sstd_arima@sigma.t, 'psstd', nu=7.332e+00, xi=9.568e-01)
forecast_GARCH_arima <- as.data.frame(fGarch::predict(garch_sstd_arima, n.ahead = 5, plot=T))
forecast_GARCH_arima

# forecast: prices (logarithmic)
forecast_arima_log_prices <- as.data.frame(forecast::forecast(arima(log(dataset[2:nrow(dataset),2]), order = c(1,1,2)), h=5, level = 0.95))
sd_times_cv <- forecast_GARCH_arima[,1] - forecast_GARCH_arima[,4]
forecast_arima_log_prices[,4] <- forecast_arima_log_prices[,1] - sd_times_cv
forecast_arima_log_prices[,5] <- forecast_arima_log_prices[,1] + sd_times_cv
forecast_arima_log_prices

# forecast: prices (logarithmic) while EX: e^mi+(sd^2/2)
# Lo_95 = mi - sd*crit_value => sd = (mi - Lo_95) / crit_value
sd <- (forecast_arima_log_prices$`Point Forecast` - forecast_arima_log_prices$`Lo 95`)/1.96
forecast_arima_prices <- exp(forecast_arima_log_prices$`Point Forecast`+0.5*sd^2)
forecast_arima_prices <- as.data.frame(forecast_arima_prices)
forecast_arima_prices[,2:5] <- exp(forecast_arima_log_prices[,2:5])
forecast_arima_prices

april <- read.csv('chfeur_d_april.csv')
ME <- round(mean((april$Zamkniecie-forecast_arima_prices[,1])),5)
MAE <- round(mean(abs(april$Zamkniecie-forecast_arima_prices[,1])),5)
MSE <- round(mean((april$Zamkniecie-forecast_arima_prices[,1])^2),5)
MAPE <- round(mean(abs((april$Zamkniecie-forecast_arima_prices[,1])/april$Zamkniecie)),5)
