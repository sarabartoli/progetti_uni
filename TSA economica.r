#importo il dataset, i pacchetti e i file
RSGASSN = read.csv('~/Statistica economica/Progetto/RSGASSN.csv')
source('C:/Users/barto/Documents/Statistica economica/Esercizi/TSA-Predict-Student-Functions.R')
source('C:/Users/barto/Documents/Statistica economica/Esercizi/TSA-Useful-Functions.R')
library(forecast)
library(tseries)
library(tsoutliers)
library(urca)
#date
RSGASSN$TIME = as.Date(x = as.character(RSGASSN$DATE), format = '%Y-%m-%d')
#creo oggetto time series
start = as.numeric(c(format(RSGASSN$TIME[1], '%Y'), format(RSGASSN$TIME[1], '%m')))
TS=ts(data = RSGASSN$RSGASSN, start = start, frequency = 12) #time series completa
timeFull=RSGASSN$TIME
TS_LOG=ts(data = log(RSGASSN$RSGASSN), start = start, frequency = 12) # log time series completa
TS_SQRT=ts(data = sqrt(RSGASSN$RSGASSN), start = start, frequency = 12) #√ time series completa
ts12 = ts( data = TS[1:(length(TS)-12)], start = start, frequency = 12) #time series senza l'ultimo anno per previsioni ex-post
tslog = ts( data = TS_LOG[1:(length(TS_LOG)-12)], start = start, frequency = 12)
tssqrt = ts( data = TS_SQRT[1:(length(TS_SQRT)-12)], start = start, frequency = 12)

#time series
#analisi prelimiari
par(mfrow = c(3,1))
plot(ts12, type = 'l', main = 'Vendite al dettaglio distributori benzina', ylab = '')
Acf(x = ts12, type = 'correlation', na.action = na.pass, lag.max = 60, main = 'Vendite al dettaglio distributori benzina')
Acf(x = ts12, type = 'partial', na.action = na.pass, lag.max = 60, main = 'Vendite al dettaglio distributori benzina')

#test ADF
adf.1 = ur.df(y = ts12, type = 'trend', lags = 24, selectlags = 'AIC')
adf.1@teststat
adf.1@cval #non rifiuto tau3 e phi3, controllo phi2: non rifiuto
adf.2 = ur.df(y = ts12, type = 'drift', lags = 24, selectlags = 'AIC') 
adf.2@teststat
adf.2@cval #non rifiuto tau2 e phi1, concludo che c'è unit root

#arima
#ARIMA(0,1,0)(0,1,0)[12] AIC=5715.23 BIC=5719.01
#ARIMA(0,1,0)(0,1,1)[12] AIC=5564.38 BIC=5571.93
#ARIMA(1,1,0)(0,1,1)[12] AIC=5536.66 BIC=5547.99
#ARIMA(0,1,1)(0,1,1)[12] AIC=5533.46 BIC=5544.79
#ARIMA(1,0,1)(0,1,1)[12] +C AIC=5545.77 BIC=5564.67

fit = Arima(y = ts12, 
  order = c(1, 0, 1), 
  seasonal = list(order = c(0, 1, 1), period = 12),
  xreg = NULL, include.constant = TRUE)
summary(fit)
fitts=fit

#root analysis
par(mfrow = c(1,1))
root = .arma.roots(fit = fit)
.circle(win = 2.7)
points(root$root$ar, col = 'red')
points(root$root$ma, col = 'blue')

num_par = NROW(fit$coef)
lag1 = num_par + c(1, 2, 5, 7, 8, 9, 10)
residui = residuals(fit)
residui_stand = (residui - mean(residui)) / sqrt(fit$sigma2)

#ts plot, acf, pacf sui residui
par(mfrow = c(3,1))
plot(residui, type = 'l', main = 'Residui', ylab = '')
Acf(x = residuals(fit), type = 'correlation', na.action = na.pass, lag.max = 60, main = 'Residui')
Acf(x = residuals(fit), type = 'partial', na.action = na.pass, lag.max = 60, main = 'Residui')
#ljung-box
mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = residui, type = 'Ljung-Box', fitdf = num_par))[1:3, , drop = FALSE]

#ts plot, acf, pacf residui²
par(mfrow = c(3,1))
x1 = residui^2
plot(x1, type = 'l', main = 'Residui²', ylab = '')
Acf(x = x1, type = 'correlation', lag.max = 60, na.action = na.pass, main = 'Residui²')
Acf(x = x1, type = 'partial',     lag.max = 60, na.action = na.pass, main = 'Residui²')
#ljung-box
mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = residui^2, type = 'Ljung-Box', fitdf = num_par))[1:3, , drop = FALSE]

#ts plot, acf, pacf |residui|
par(mfrow = c(3,1))
x1 = abs(residui)
plot(x1, type = 'l', main = '|Residui|', ylab = '')
Acf(x = x1, type = 'correlation', lag.max = 60, na.action = na.pass, main = '|Residui|')
Acf(x = x1, type = 'partial', lag.max = 60, na.action = na.pass, main = '|Residui|')
#ljung-box
mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = abs(residui), type = 'Ljung-Box', fitdf = num_par))[1:3, , drop = FALSE]

# outliers
include.constant = any(names(fit$coef) %in% c('intercept', 'drift'))
order = fit$arma[c(1,6,2)]
seasonal = list(order = fit$arma[c(3,7,4)], period = fit$arma[5])
fit_out = tso(y = ts12,
  xreg=NULL, 
  types = c('AO', 'LS', 'TC'), delta = 0.7, cval = 3.75, 
  maxit = 10, maxit.iloop = 100, maxit.oloop = 10, 
  tsmethod = 'arima', 
  args.tsmethod = list(order = order,  seasonal = seasonal))
plot(fit_out)
print(fit_out)
fittsout=fit_out
out_eff = NULL
if (NROW(fit_out$outliers) > 0)
{
  out_eff = outliers.effects(mo = fit_out$outliers, n = NROW(ts12), pars = coef(fit_out$fit), 
    weights = FALSE)
  out_eff = as.matrix(out_eff)
}

# arima con outliers
fit = Arima(y = ts12, 
  order = order, seasonal = seasonal, include.constant = include.constant, 
  xreg = out_eff)
fittsoutar=fit

# root analysis
par(mfrow = c(1,1))
root = .arma.roots(fit = fit)
.circle(win = 4.5)
points(root$root$ar, col = 'red')
points(root$root$ma, col = 'blue')

num_par = NROW(fit$coef)
lag1 = num_par + c(1, 2, 5, 7, 8, 9, 10)
residui = residuals(fit)
residui_stand = (residui - mean(residui)) / sqrt(fit$sigma2)

# ts plot, acf, pacf sui residui
par(mfrow = c(3,1))
plot(residuals(fit), type = 'l', main = 'Residui', ylab = '')
Acf(x = residuals(fit), type = 'correlation', na.action = na.pass, lag.max = 60, main = 'Residui')
Acf(x = residuals(fit), type = 'partial', na.action = na.pass, lag.max = 60, main = 'Residui')
# ljung-box
mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = residui, type = 'Ljung-Box', fitdf = num_par))[1:3, , drop = FALSE]

# ts plot, acf, pacf residui²
par(mfrow = c(3,1))
x1 = residui^2
plot(x1, type = 'l', main = 'Residui²', ylab = '')
Acf(x = x1, type = 'correlation', lag.max = 60, na.action = na.pass, main = 'Residui²')
Acf(x = x1, type = 'partial', lag.max = 60, na.action = na.pass, main = 'Residui²')
# ljung-box
mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = x1, type = 'Ljung-Box', fitdf = num_par))[1:3, , drop = FALSE]

# ts plot, acf, pacf |residui|
par(mfrow = c(3,1))
x1 = abs(residui)
plot(x1, type = 'l', main = '|Residui|', ylab = '')
Acf(x = x1, type = 'correlation', lag.max = 60, na.action = na.pass, main = '|Residui|')
Acf(x = x1, type = 'partial', lag.max = 60, na.action = na.pass, main = '|Residui|')
mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = x1, type = 'Ljung-Box', fitdf = num_par))[1:3, , drop = FALSE]

#distribuzione residui
par(mfrow = c(1,2))
hist(x = residui_stand, breaks = 30, freq = FALSE, main = 'Residui', xlab = '')
x1 = seq(from = min(residui_stand), to = max(residui_stand)+1, length.out = 100) 
lines(x = x1, y = dnorm(x = x1, mean = 0, sd = 1), col = 'red')
qqnorm(y = residui_stand, main = 'Normal Q-Q Plot',
  xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles',
  plot.it = TRUE)
abline(a = 0, b = 1) #si evidenzia non normalità

#time series sqrt
#analisi prelimiari
par(mfrow = c(3,1))
plot(tssqrt, type = 'l', main = 'Vendite al dettaglio distributori benzina', ylab = '')
Acf(x = tssqrt, type = 'correlation', na.action = na.pass, lag.max = 60, main = 'Vendite al dettaglio distributori benzina')
Acf(x = tssqrt, type = 'partial', na.action = na.pass, lag.max = 60, main = 'Vendite al dettaglio distributori benzina')

#test ADF
adf.1 = ur.df(y = tssqrt, type = 'trend', lags = 24, selectlags = 'AIC')
adf.1@teststat
adf.1@cval #non rifiuto tau3 e phi3, controllo phi2: non rifiuto
adf.2 = ur.df(y = tssqrt, type = 'drift', lags = 24, selectlags = 'AIC') 
adf.2@teststat
adf.2@cval #non rifiuto tau2 e phi1, concludo che c'è unit root

#arima
fit = Arima(y = tssqrt, 
  order = c(1, 0, 1), 
  seasonal = list(order = c(0, 1, 1), period = 12),
  xreg = NULL, include.constant = TRUE)
summary(fit)
.loglik(fit=fit, g='sqrt')
fitsqrt=fit

par(mfrow = c(3,1))
x1 = abs(residuals(fitsqrt))
plot(x1, type = 'l', main = '|Residui|', ylab = '')
Acf(x = x1, type = 'correlation', lag.max = 60, na.action = na.pass, main = '|Residui|')
Acf(x = x1, type = 'partial', lag.max = 60, na.action = na.pass, main = '|Residui|')

#root analysis
par(mfrow = c(1,1))
root = .arma.roots(fit = fit)
.circle(win = 1.5)
points(root$root$ar, col = 'red')
points(root$root$ma, col = 'blue')

#ts plot, acf, pacf sui residui
par(mfrow = c(3,1))
plot(residuals(fit), type = 'l', main = 'Residui', ylab = '')
Acf(x = residuals(fit), type = 'correlation', na.action = na.pass, lag.max = 60, main = 'Residui')
Acf(x = residuals(fit), type = 'partial', na.action = na.pass, lag.max = 60, main = 'Residui')

#outliers
include.constant = any(names(fit$coef) %in% c('intercept', 'drift'))
order = fit$arma[c(1,6,2)]
seasonal = list(order = fit$arma[c(3,7,4)], period = fit$arma[5])
fit_out = tso(y = tssqrt,
  xreg=NULL, 
  types = c('AO', 'LS', 'TC'), delta = 0.7, cval = 4.25, 
  maxit = 10, maxit.iloop = 100, maxit.oloop = 10, 
  tsmethod = 'arima', 
  args.tsmethod = list(order = order,  seasonal = seasonal))
plot(fit_out)
print(fit_out)
fitsqrtout=fit_out
out_eff = NULL
if (NROW(fit_out$outliers) > 0)
{
  out_eff = outliers.effects(mo = fit_out$outliers, n = NROW(tssqrt), pars = coef(fit_out$fit), 
    weights = FALSE)
  out_eff = as.matrix(out_eff)
}

#arima con outliers
fit = Arima(y = tssqrt, 
  order = order, seasonal = seasonal, include.constant = include.constant, 
  xreg = out_eff)
.loglik(fit=fit, g='sqrt')
fitsqrtoutar=fit

#root analysis
par(mfrow = c(1,1))
root = .arma.roots(fit = fit)
.circle(win = 1.5)
points(root$root$ar, col = 'red')
points(root$root$ma, col = 'blue')

num_par = NROW(fit$coef)
lag1 = num_par + c(1, 2, 5, 7, 8, 9, 10)
residui = residuals(fit)
residui_stand = (residui - mean(residui)) / sqrt(fit$sigma2)

#ts plot, acf, pacf sui residui
par(mfrow = c(3,1))
plot(residuals(fit), type = 'l', main = 'Residui', ylab = '')
Acf(x = residuals(fit), type = 'correlation', na.action = na.pass, lag.max = 60, main = 'Residui')
Acf(x = residuals(fit), type = 'partial', na.action = na.pass, lag.max = 60, main = 'Residui')
#ljung-box
mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = residui, type = 'Ljung-Box', fitdf = num_par))[1:3, , drop = FALSE]

#ts plot, acf, pacf residui²
par(mfrow = c(3,1))
x1 = residui^2
plot(x1, type = 'l', main = 'Residui²', ylab = '')
Acf(x = x1, type = 'correlation', lag.max = 60, na.action = na.pass, main = 'Residui²')
Acf(x = x1, type = 'partial',     lag.max = 60, na.action = na.pass, main = 'Residui²')
#ljung-box
mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = x1, type = 'Ljung-Box', fitdf = num_par))[1:3, , drop = FALSE]

#ts plot, acf, pacf |residui|
par(mfrow = c(3,1))
x1 = abs(residui)
plot(x1, type = 'l', main = '|Residui|', ylab = '')
Acf(x = x1, type = 'correlation', lag.max = 60, na.action = na.pass, main = '|Residui|')
Acf(x = x1, type = 'partial', lag.max = 60, na.action = na.pass, main = '|Residui|')
mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = x1, type = 'Ljung-Box', fitdf = num_par))[1:3, , drop = FALSE]

#distribuzione residui
par(mfrow = c(1,2))
hist(x = residui_stand, breaks = 30, freq = FALSE, main = 'Residui', xlab = '')
x1 = seq(from = min(residui_stand), to = max(residui_stand)+1, length.out = 100) 
lines(x = x1, y = dnorm(x = x1, mean = 0, sd = 1), col = 'red')
qqnorm(y = residui_stand, main = 'Normal Q-Q Plot',
  xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles',
  plot.it = TRUE)
abline(a = 0, b = 1)

#diagnostica trasformazione
summary(lm(log(residuals(fittsoutar)^2)~log(fitted.values(fittsoutar))))
1-pt((3.3875-1)/0.3107,334)

#time series log
#analisi prelimiari
par(mfrow = c(3,1))
plot(tslog, type = 'l', main = 'Vendite al dettaglio distributori benzina', ylab = '')
Acf(x = tslog, type = 'correlation', na.action = na.pass, lag.max = 60, main = 'Vendite al dettaglio distributori benzina')
Acf(x = tslog, type = 'partial', na.action = na.pass, lag.max = 60, main = 'Vendite al dettaglio distributori benzina')

#test ADF
adf.1 = ur.df(y = tslog, type = 'trend', lags = 24, selectlags = 'AIC')
adf.1@teststat
adf.1@cval #non rifiuto tau3 e phi3, controllo phi2: non rifiuto
adf.2 = ur.df(y = tslog, type = 'drift', lags = 24, selectlags = 'AIC') 
adf.2@teststat
adf.2@cval #non rifiuto tau2 e phi1, concludo che c'è unit root

#arima
fit = Arima(y = tslog, 
  order = c(1, 0, 1), 
  seasonal = list(order = c(0, 1, 1), period = 12),
  xreg = NULL, include.constant = TRUE)
summary(fit)
.loglik(fit=fit, g='log')
fitlog=fit

#root analysis
par(mfrow = c(1,1))
root = .arma.roots(fit = fit)
.circle(win = 1.5)
points(root$root$ar, col = 'red')
points(root$root$ma, col = 'blue')

#ts plot, acf, pacf sui |residui|
par(mfrow = c(3,1))
plot(abs(residuals(fit)), type = 'l', main = '|Residui|', ylab = '')
Acf(x = abs(residuals(fit)), type = 'correlation', na.action = na.pass, lag.max = 60, main = '|Residui|')
Acf(x = abs(residuals(fit)), type = 'partial', na.action = na.pass, lag.max = 60, main = '|Residui|')

#outliers
include.constant = any(names(fit$coef) %in% c('intercept', 'drift'))
order = fit$arma[c(1,6,2)]
seasonal = list(order = fit$arma[c(3,7,4)], period = fit$arma[5])
fit_out = tso(y = tslog,
  xreg=NULL, 
  types = c('AO', 'LS', 'TC'), delta = 0.7, cval = 4.75, 
  maxit = 10, maxit.iloop = 100, maxit.oloop = 10, 
  tsmethod = 'arima', 
  args.tsmethod = list(order = order,  seasonal = seasonal))
plot(fit_out)
print(fit_out)
fitlogout=fit_out
out_eff = NULL
if (NROW(fit_out$outliers) > 0)
{
  out_eff = outliers.effects(mo = fit_out$outliers, n = NROW(tslog), pars = coef(fit_out$fit), 
    weights = FALSE)
  out_eff = as.matrix(out_eff)
}

#arima con outliers
fit = Arima(y = tslog, 
  order = order, seasonal = seasonal, include.constant = include.constant, 
  xreg = out_eff)
summary(fit)
.loglik(fit=fit, g='log')
fitlogoutar=fit

#root analysis
par(mfrow = c(1,1))
root = .arma.roots(fit = fit)
.circle(win = 1.5)
points(root$root$ar, col = 'red')
points(root$root$ma, col = 'blue')

num_par = NROW(fit$coef)
lag1 = num_par + c(1, 2, 5, 7, 8, 9, 10)
residui = residuals(fit)
residui_stand = (residui - mean(residui)) / sqrt(fit$sigma2)

#ts plot, acf, pacf sui residui
par(mfrow = c(3,1))
plot(residuals(fit), type = 'l', main = 'Residui', ylab = '')
Acf(x = residuals(fit), type = 'correlation', na.action = na.pass, lag.max = 60, main = 'Residui')
Acf(x = residuals(fit), type = 'partial', na.action = na.pass, lag.max = 60, main = 'Residui')
#ljung-box
mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = residui, type = 'Ljung-Box', fitdf = num_par))[1:3, , drop = FALSE]

#ts plot, acf, pacf residui²
par(mfrow = c(3,1))
x1 = residui^2
plot(x1, type = 'l', main = 'Residui²', ylab = '')
Acf(x = x1, type = 'correlation', lag.max = 60, na.action = na.pass, main = 'Residui²')
Acf(x = x1, type = 'partial', lag.max = 60, na.action = na.pass, main = 'Residui²')
#ljung-box
mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = x1, type = 'Ljung-Box', fitdf = num_par))[1:3, , drop = FALSE]

#ts plot, acf, pacf |residui|
par(mfrow = c(3,1))
x1 = abs(residui)
plot(x1, type = 'l', main = '|Residui|', ylab = '')
Acf(x = x1, type = 'correlation', lag.max = 60, na.action = na.pass, main = '|Residui|')
Acf(x = x1, type = 'partial', lag.max = 60, na.action = na.pass, main = '|Residui|')
mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = x1, type = 'Ljung-Box', fitdf = num_par))[1:3, , drop = FALSE]

#distribuzione residui
par(mfrow = c(1,2))
hist(x = residui_stand, breaks = 30, freq = FALSE, main = 'Residui', xlab = '')
x1 = seq(from = min(residui_stand), to = max(residui_stand)+1, length.out = 100) 
lines(x = x1, y = dnorm(x = x1, mean = 0, sd = 1), col = 'red')
qqnorm(y = residui_stand, main = 'Normal Q-Q Plot',
  xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles',
  plot.it = TRUE)
abline(a = 0, b = 1)
jarque.bera.test(x=residui)

#previsioni ex post
J = 12
H = 1
t1 = .predict.t1(nobs = NROW(TS), J = J, n.ahead = H)

#modello trasformazione log + outliers
x2 = .oeff.4.predict(object = fitlogout, n.ahead = J, type = '.')
pred_log_out = .predict(object = fitlogoutar, n.ahead = H, t = t1, y = TS_LOG, xreg = as.matrix(x2), fixed.n.ahead = TRUE)
#previsioni naive
predn_ts = .predict.naive(fit = fitlogoutar, J = J, n.ahead = H)
#errori
p_log_out = .ErrorMeasures(y = TS, fit = exp(pred_log_out$pred[, 'mean']+(pred_log_out$pred$se^2)/2), naive = exp(predn_ts))
n = .ErrorMeasures(y = TS, fit = exp(predn_ts), naive = exp(predn_ts))
errori = data.frame(
  model = c('Arima + Outliers (log)', 'Naive'), h = H,
  rbind( p_log_out, n, deparse.level = 0))
print(errori)
 
#plot
ind  = (NROW(TS) - J + 1) : NROW(TS)
ind1 = 1 : NROW(ind)
x1 = .pred.bands(pred = pred_log_out, alpha = 0.05, g='log')
predn=exp(predn_ts)#+n[3]/2)
par(mfrow = c(1,1))
plot(x = timeFull[ind], y = TS[ind], xlab = 'Mesi', ylab = 'Milioni di $', 
  main = 'Previsioni (h = 1) dei passati 12 mesi', 
  ylim = range(range(25000, 60000)))
lines(x = timeFull[ind], y = x1$mean[ind1], col = 'red')
lines(x = timeFull[ind], y = predn[ind1], col = 'green')
lines(x = timeFull[ind], y = x1$lower[ind1], col = 'red', lty = 'dotted')
lines(x = timeFull[ind], y = x1$upper[ind1], col='red', lty = 'dotted')
legend('topright', c('Arima+outl.','Naive'), lty=1, col=c('red','green'))

#previsioni ex post h=6
J_6 = 12
H_6 = 6
t1_6 = .predict.t1(nobs = NROW(TS), J = J_6, n.ahead = H_6)

#modello trasformazione log + outliers
x2_6 = .oeff.4.predict(object = fitlogout, n.ahead = J_6, type = '.')
pred_log_out_6 = .predict(object = fitlogoutar, n.ahead = H_6, t = t1_6, y = TS_LOG, xreg = as.matrix(x2_6), fixed.n.ahead = TRUE)
#previsioni naive
predn_ts_6 = .predict.naive(fit = fitlogoutar, J = J_6, n.ahead = H_6)

#errori
p_log_out_6 = .ErrorMeasures(y = TS, fit = exp(pred_log_out_6$pred[, 'mean']+(pred_log_out_6$pred$se^2)/2), naive = exp(predn_ts_6))
n_6 = .ErrorMeasures(y = TS, fit = exp(predn_ts_6), naive = exp(predn_ts_6))
errori_6 = data.frame(
  model = c('Arima + Outliers (log)' ,'Naive'), h = H_6,
  rbind( p_log_out_6, n_6, deparse.level = 0))
print(errori_6)
 
#plot
ind_6  = (NROW(TS) - J + 1) : NROW(TS)
ind1_6 = 1 : NROW(ind)
x1_6 = .pred.bands(pred = pred_log_out_6, alpha = 0.05, g='log')
predn_6=exp(predn_ts_6)#+n_6[3]/2)
par(mfrow = c(1,1))
plot(x = timeFull[ind], y = TS[ind], xlab = 'Mesi', ylab = 'Milioni di $', 
  main = 'Previsioni (h = 6) dei passati 12 mesi' ,ylim = range(25000, 60000))
lines(x = timeFull[ind], y = x1_6$mean[ind1_6], col = 'red')
lines(x = timeFull[ind], y = predn_6[ind1_6], col = 'green')
lines(x = timeFull[ind], y = x1_6$lower[ind1_6], col = 'red', lty = 'dotted')
lines(x = timeFull[ind], y = x1_6$upper[ind1_6], col='red', lty = 'dotted')
legend('topright', c('Arima+outl.','Naive'), lty=1, col=c('red','green'))

#modello trasformazione log + outliers time series completa
order = fitlog$arma[c(1,6,2)]
seasonal = list(order = fitlog$arma[c(3,7,4)], period = fitlog$arma[5])
fit_out = tso(y = TS_LOG,
  xreg=NULL, 
  types = c('AO', 'LS', 'TC'), delta = 0.7, cval = 4.75, 
  maxit = 10, maxit.iloop = 100, maxit.oloop = 10, 
  tsmethod = 'arima', 
  args.tsmethod = list(order = order,  seasonal = seasonal))
plot(fit_out)
print(fit_out)
out_eff = NULL
if (NROW(fit_out$outliers) > 0)
{
  out_eff = outliers.effects(mo = fit_out$outliers, n = NROW(TS_LOG), pars = coef(fit_out$fit), 
    weights = FALSE)
  out_eff = as.matrix(out_eff)
}
m_logoutar = Arima(y = TS_LOG, 
  order = fitlogoutar$arma[c(1, 6, 2)], seasonal = list(order = fitlogoutar$arma[c(3, 7, 4)]),
  xreg = out_eff, include.constant = TRUE)
summary(m_logoutar)

#previsioni ex ante
H = 12 
t1 = NROW(TS_LOG)
#con outliers
outliers=.oeff.4.predict(object= fit_out, n.ahead=H+NROW(TS_LOG)+NROW(tslog), type='.')
prev_log_out = .predict(object = m_logoutar, n.ahead = H, t = t1, y = TS_LOG, xreg = outliers, fixed.n.ahead = FALSE)
#previsioni e IC
previsione=exp(prev_log_out$pred$mean+(prev_log_out$pred$se^2)/2)
UIC=exp(prev_log_out$pred$mean+qnorm(0.975)*prev_log_out$pred$se
)
LIC=exp(prev_log_out$pred$mean-qnorm(0.975)*prev_log_out$pred$se)
cbind(c('','Gennaio','Febbraio','Marzo','Aprile','Maggio','Giugno','Luglio','Agosto','Settembre','Ottobre','Novembre','Dicembre'),
c('Lower IC',round(LIC)),
c('Previsione',round(previsione)),
c('Upper IC',round(UIC)))

#grafico
ind  = (NROW(TS) - J + 1) : NROW(TS)
date=c('2021-01-01', '2021-02-01', '2021-03-01', '2021-04-01', '2021-05-01', '2021-06-01', '2021-07-01', '2021-08-01', '2021-09-01', '2021-10-01', '2021-11-01', '2021-12-01')
date=as.Date(x = date, format = '%Y-%m-%d')
par(mfrow = c(1,1))
plot(x = c(timeFull[ind],date[1]), y = c(TS[ind],previsione[1]), type='l' ,xlab = 'Tempo', ylab = 'Milioni di $', 
  ylim = range(range(25000, 60000)), xlim=range(timeFull[ind][1],date[length(date)]))
lines(x=date ,y=previsione, col='red')
lines(x=date ,y=UIC, col='red', lty = 'dotted')
lines(x=date ,y=LIC, col='red', lty = 'dotted')