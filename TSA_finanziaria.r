#importo il dataset, i pacchetti e i file
library(tseries)  
library(sandwich)
library(lmtest)
library(urca)
library(rugarch)
library(FinTS)
library(car)
library(forecast)
library(xts)
source('C:/Users/barto/Documents/Statistica economica/Esercizi/TSA-Predict-Student-Functions.R')
source('C:/Users/barto/Documents/Statistica economica/Esercizi/TSA-Finance-Functions.R')
source('C:/Users/barto/Documents/Statistica economica/Esercizi/TSA-Useful-Functions.R')
data=read.csv('~/Statistica economica/Progetto/DPZ.csv')
data = data.frame(data, cc.ret = c(NA, diff(log(data$Adjusted))), 
gkVol = .garmanklass(data = data, sd = TRUE), check.names = TRUE)
time = as.Date(x = data$Date)
yc = data$Close
yclog = log(yc)
y = data$Adjusted
ylog = log(y)
nobs = NROW(y)

#analisi dei prezzi
#grafici
par(mfrow = c(2,2))
plot(x = time, y = yc, main = 'Close', xlab = '', ylab = '', type = 'l')
plot(x = time, y = yclog, main = 'Ln(close)', xlab = '', ylab = '', type = 'l')
plot(x = time, y = y, main = 'AdjClose', xlab = '', ylab = '', type = 'l')
plot(x = time, y = ylog, main = 'Ln(AdjClose)', xlab = '', ylab = '', type = 'l')
par(mfrow = c(2,1))
Acf(x = ylog, lag.max = 100, type = 'correlation', main = 'Price')
Acf(x = ylog, lag.max = 100, type = 'partial', main = 'Price')

#adf di fonzo-lisi
adf.1 = ur.df(y = ylog, type = 'trend', lags = 20, selectlags = 'AIC')
adf.1@teststat
adf.1@cval #non rifiuto tau3, phi3. rifiuto phi2.
adf.2=.ur.drift(y=ylog, lags=20, selectlags='AIC')
adf.2@teststat
adf.2@cval #non rifiuto, ho unit root

#analisi preliminari
yret = xts(x = 100 * data$cc.ret, order.by = time)[-1] #log returns in %
time=time[-1]
par(mfrow = c(1,1))
plot(x = time, y = yret, main = 'Returns', xlab = '', ylab = '', type = 'l')
#acf
par(mfrow = c(2,1))
Acf(x = yret, lag.max = 100, type = 'correlation', main = 'Returns')
Acf(x = yret, lag.max = 100, type = 'partial', main = 'Returns')
#ljung-box
npar = 0
lag = c(2, 5, 10, 15, 20, 30, 50) + npar
lb = mapply(FUN = Box.test, lag = lag, MoreArgs = list(x = yret, type = 'Ljung-Box', fitdf = npar))[1:3,]
print(rbind(lag = lag, lb))
#adf
adf.1 = ur.df(y = data$cc.ret[-1], type = 'trend', lags = 20, selectlags = 'AIC')
adf.1@teststat
adf.1@cval #no unit root (rifiuto tau3 e phi3)
#test bds indipendenza sui returns
x1 = yret
bds.test(x = x1, m = 4, eps = seq(from = 0.5 * sd(x1), to = 2 * sd(x1), length = 4), trace = FALSE) #rifiuto, non IID
#distribuzione dei returns
par(mfrow = c(1,2))
.hist(x = yret, xlim = c(-10, 10), n = 200, breaks = 200, main = 'Returns')
qqnorm(y = scale(yret))
abline(a = 0, b = 1, col = 'red') #si rifiuta normalità
#jarque-bera
jarque.bera.test(x = yret)
#test arch
lag = c(4, 8, 12, 16)
at = mapply(FUN = ArchTest, lags = lag, MoreArgs = list(x = yret, demean = TRUE))
print(at[1:3,]) #eteroschedasticità condizionata

#arma su log returns
spec0 = arfimaspec( mean.model = list(armaOrder = c(1,0), include.mean = TRUE, external.regressors = NULL), distribution.model = 'std') 
fit0 = arfimafit(spec = spec0, data = yret, solver = 'solnp')
infocriteria(fit0)
fit0@fit$matcoef
fit0@fit$robust.matcoef

#acf su residui, residui², |residui|
res = as.numeric(residuals(fit0))
par(mfrow = c(3,1))
Acf(x = res, lag.max = 100, type = 'correlation', main = 'Residui')
Acf(x = abs(res), lag.max = 100, type = 'correlation', main = '|Residui|')
Acf(x = res^2, lag.max = 100, type = 'correlation', main = 'Residui²') #no WN

#simple garch
spec1 = ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1), submodel = NULL, external.regressors = NULL, variance.targeting = FALSE), mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, external.regressors = NULL), distribution.model = 'std')
fit1 = ugarchfit(spec = spec1, data = yret, solver = 'solnp')
np1 = NROW(fit1@fit$coef)
infocriteria(fit1)
fit1@fit$matcoef
fit1@fit$robust.matcoef

#stabilità
nyblom(fit1) #stabile

#diagnostiche su residui standardizzati
fit = fit1
par(mfrow = c(3,1))
Acf(x = fit@fit$z,      lag.max = 100, type = 'correlation', main = 'z')
Acf(x = abs(fit@fit$z), lag.max = 100, type = 'correlation', main = '|z|')
Acf(x = fit@fit$z^2,    lag.max = 100, type = 'correlation', main = expression(z^2))
lag1 = np1 + c(1, 2, 5, 10, 15, 20)
#ljung-box residui standardizzati
lb1 = mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = fit@fit$z, type = 'Ljung-Box', fitdf = np1))
print(rbind(lag = lag1, lb1[1:3,]))
#ljung-box |residui standardizzati|
lb1 = mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = abs(fit@fit$z), type = 'Ljung-Box', fitdf = np1))
print(rbind(lag = lag1, lb1[1:3,]))
#ljung-box residui² standardizzati
lb1 = mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = fit@fit$z^2, type = 'Ljung-Box', fitdf = np1))
print(rbind(lag = lag1, lb1[1:3,]))

#test arch
lag = c(4, 8, 12, 16)
at = mapply(FUN = ArchTest, lags = lag, MoreArgs = list(x = fit1@fit$z, demean = TRUE))
print(at[1:3,]) #omoschedasticità condizionata 

#distribuzione residui
par(mfrow = c(1,2))
xlim = c(-5, 5)
.hist.fit(fit = fit1, xlim = xlim, ylim = c(0,0.55), n = 200, breaks = 100, plot.norm = TRUE, main = '')
.qqplot.fit(fit = fit1)

#test bds su |residui standardizzati|
x1 = log(abs(fit@fit$z))
bds = bds.test(x = x1, m = 4, eps = seq(from = 0.5 * sd(x1), to = 2 * sd(x1), length = 4), trace = FALSE)
print(bds) #returns IID

#leverage
signbias(fit1) #non significativo ma test non potente

#gjr garch
spec2 = ugarchspec(variance.model = list(model = 'gjrGARCH', garchOrder = c(1,1), submodel = NULL, external.regressors = NULL, variance.targeting = FALSE), mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, external.regressors = NULL), distribution.model = 'std')
fit2 = ugarchfit(spec = spec2, data = yret, solver = 'solnp')
infocriteria(fit2)
fit2@fit$matcoef
fit2@fit$robust.matcoef #gamma significativo
np2 = NROW(fit2@fit$coef)

#News Impact Curves sGARCH gjrGARCH 
ni1 = newsimpact(z = NULL, fit1)
ni2 = newsimpact(z = NULL, fit2)
legend = c('Simple-GARCH', 'GJR-GARCH')
col = c('black', 'red')
ylim = range( ni1$zy, ni2$zy )
par(mfrow = c(1,1), mar = c(4, 4.5, 3, 1) + 0.1)
plot(x = ni1$zx, y = ni1$zy, ylab = ni1$yexpr, xlab = ni1$xexpr, type = 'l', ylim = ylim, main = 'News Impact Curve', col = col[1])
lines(x = ni2$zx, y = ni2$zy, col = col[2])
legend(x = 'topright', y = NULL, legend = legend, border = FALSE, col = col, lty = 1, text.col = col)

#stabilità
nyblom(fit2) #non stabile

#diagnostiche su residui standardizzati
fit = fit2
par(mfrow = c(3,1))
Acf(x = fit@fit$z,      lag.max = 100, type = 'correlation', main = 'z')
Acf(x = abs(fit@fit$z), lag.max = 100, type = 'correlation', main = '|z|')
Acf(x = fit@fit$z^2,    lag.max = 100, type = 'correlation', main = expression(z^2))
lag1 = np2 + c(1, 2, 5, 10, 15, 20)
#ljung-box residui standardizzati
lb1 = mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = fit@fit$z, type = 'Ljung-Box', fitdf = np2))
print(rbind(lag = lag1, lb1[1:3,]))
#ljung-box |residui standardizzati|
lb1 = mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = abs(fit@fit$z), type = 'Ljung-Box', fitdf = np2))
print(rbind(lag = lag1, lb1[1:3,]))
#ljung-box residui² standardizzati
lb1 = mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = fit@fit$z^2, type = 'Ljung-Box', fitdf = np2))
print(rbind(lag = lag1, lb1[1:3,]))

#test arch
lag = c(4, 8, 12, 16)
at = mapply(FUN = ArchTest, lags = lag, MoreArgs = list(x = fit2@fit$z, demean = TRUE))
print(at[1:3,]) #omoschedasticità condizionata 

#distribuzione residui
par(mfrow = c(1,2))
xlim = c(-5, 5)
.hist.fit(fit = fit2, xlim = xlim, ylim = c(0,0.55), n = 200, breaks = 100, plot.norm = TRUE, main = '')
.qqplot.fit(fit = fit2)

#test bds su |residui standardizzati|
x1 = log(abs(fit@fit$z))
bds = bds.test(x = x1, m = 4, eps = seq(from = 0.5 * sd(x1), to = 2 * sd(x1), length = 4), trace = FALSE)
print(bds) #returns IID

#tgarch in fgarch
spec5 = ugarchspec(variance.model = list(model = 'fGARCH', garchOrder = c(1, 1), submodel = 'TGARCH', external.regressors = NULL, variance.targeting = FALSE), mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, external.regressors = NULL), distribution.model = 'std')
fit5 = ugarchfit(spec = spec5, data = yret, solver = 'solnp')
np5 = NROW(fit5@fit$coef)
#in forma gjr
fit5c = .fgarch.2.gjr(fit = fit5)
infocriteria(fit5)
fit5@fit$matcoef
fit5c$robust.matcoef

#News Impact Curves (sGARCH vs gjrGARCH vs TGARCH) 
ni1 = newsimpact(z = NULL, fit1)
ni2 = newsimpact(z = NULL, fit2)
ni5 = newsimpact(z = NULL, fit5)
legend = c('Simple-GARCH', 'GJR-GARCH', 'T-GARCH')
col  = c('black', 'red', 'blue')
ylim = range( ni1$zy, ni2$zy, ni5$zy)
par(mfrow = c(1,1), mar = c(4, 4.5, 3, 1) + 0.1)
plot(x = ni1$zx, y = ni1$zy, ylab = ni1$yexpr, xlab = ni1$xexpr, type = 'l', ylim = ylim, main = 'News Impact Curve', col = col[1])
lines(x = ni2$zx, y = ni2$zy, col = col[2])
lines(x = ni5$zx, y = ni5$zy, col = col[3])
legend(x = 'topright', y = NULL, legend = legend, border = FALSE, col = col, lty = 1, text.col = col)

#stabilità
nyblom(fit5) #stabilità con significatività alpha=0.01, borderline con alpha=0.05

#diagnostiche su residui standardizzati
fit = fit5
par(mfrow = c(3,1))
Acf(x = fit@fit$z, lag.max = 100, type = 'correlation', main = 'z')
Acf(x = abs(fit@fit$z), lag.max = 100, type = 'correlation', main = '|z|')
Acf(x = fit@fit$z^2, lag.max = 100, type = 'correlation', main = expression(z^2))
lag1 = np5 + c(1, 2, 5, 10, 15, 20)
#ljung-box residui standardizzati
lb1 = mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = fit@fit$z, type = 'Ljung-Box', fitdf = np5) )
print(rbind(lag = lag1, lb1[1:3,]))
#ljung-box |residui standardizzati|
lb1 = mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = abs(fit@fit$z), type = 'Ljung-Box', fitdf = np5) )
print(rbind(lag = lag1, lb1[1:3,]))
#ljung-box residui² standardizzati
lb1 = mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = fit@fit$z^2, type = 'Ljung-Box', fitdf = np5) )
print(rbind(lag = lag1, lb1[1:3,]))

#test arch
lag = c(4, 8, 12, 16)
at = mapply(FUN = ArchTest, lags = lag, MoreArgs = list(x = fit5@fit$z, demean = TRUE))
print(at[1:3,]) #omoschedasticità condizionata 

#distribuzione residui
par(mfrow = c(1,2))
xlim = c(-5, 5)
.hist.fit(fit = fit5, xlim = xlim, ylim = c(0,0.55), n = 200, breaks = 100, plot.norm = TRUE, main = '')
.qqplot.fit(fit = fit5)

#test bds su |residui standardizzati|
x1 = log(abs(fit@fit$z))
bds = bds.test(x = x1, m = 4, eps = seq(from = 0.5 * sd(x1), to = 2 * sd(x1), length = 4), trace = FALSE)
print(bds) #returns IID

# #Igarch
# spec6 = ugarchspec(variance.model = list(model = 'iGARCH', garchOrder = c(1, 1), submodel = NULL, external.regressors = NULL, variance.targeting = FALSE), mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, arfima = FALSE, external.regressors = NULL, archex = FALSE), distribution.model = 'std')
# fit6 = ugarchfit(spec = spec6, data = yret, solver = 'solnp')
# infocriteria(fit6)
# fit6@fit$matcoef
# fit6@fit$robust.matcoef #alpha1 + beta1 = 1
# np6 = NROW(fit6@fit$coef)

# #stabilità
# nyblom(fit6) #stabile

# #diagnostiche su residui standardizzati
# fit = fit6
# par(mfrow = c(3,1))
# Acf(x = fit@fit$z, lag.max = 100, type = 'correlation', main = 'z')
# Acf(x = abs(fit@fit$z), lag.max = 100, type = 'correlation', main = '|z|')
# Acf(x = fit@fit$z^2, lag.max = 100, type = 'correlation', main = expression(z^2))
# lag1 = np6 + c(1, 2, 5, 10, 15, 20)
# #ljung-box residui standardizzati
# lb1 = mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = fit@fit$z, type = 'Ljung-Box', fitdf = np6) )
# print(rbind(lag = lag1, lb1[1:3,]))
# #ljung-box |residui standardizzati|
# lb1 = mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = abs(fit@fit$z), type = 'Ljung-Box', fitdf = np6) )
# print(rbind(lag = lag1, lb1[1:3,]))
# #ljung-box residui² standardizzati
# lb1 = mapply(FUN = Box.test, lag = lag1, MoreArgs = list(x = fit@fit$z^2, type = 'Ljung-Box', fitdf = np6) )
# print(rbind(lag = lag1, lb1[1:3,]))

# #test arch
# lag = c(4, 8, 12, 16)
# at = mapply(FUN = ArchTest, lags = lag, MoreArgs = list(x = fit6@fit$z, demean = TRUE))
# print(at[1:3,]) #omoschedasticità condizionata 

# #distribuzione residui
# par(mfrow = c(1,2))
# xlim = c(-5, 5)
# .hist.fit(fit = fit6, xlim = xlim, ylim = c(0,0.55), n = 200, breaks = 100, plot.norm = TRUE, main = '')
# .qqplot.fit(fit = fit6)

# #test bds su |residui standardizzati|
# x1 = log(abs(fit@fit$z))
# bds = bds.test(x = x1, m = 4, eps = seq(from = 0.5 * sd(x1), to = 2 * sd(x1), length = 4), trace = FALSE)
# print(bds) #returns IID

#previsioni
#confronto volatilità stimata e garmanklass con stima fatta su parte del campione incluso nella stima del garch (tgarch e igharch)
y = data$gkVol[-1]*100 #benchmark
par(mfrow = c(1,1), lwd = 1)
plot(x = time, y = y, type = 'l', ylab = 'Garman-Klass volatility measure')
lines(x = time, y = fit5@fit$sigma, col = 'blue')
lines(x = time, y = fit2@fit$sigma, col = 'red')
legend('topright', c('GARMAN - KLASS', 'GJR-GARCH', 'T-GARCH'), lty=1, col=c('black','red','blue'))

#naive della volatilità (dev. stand. dei log returns in percentuale)
naive.vol = sd(yret) 

#misure di errore
ErrorMeas = data.frame(
  measure = c('Volatility', 'Volatility', 'Volatility'), model = c('Simple-GARCH', 'GJR-GARCH', 'T-GARCH'), 
  rbind( 
    .ErrorMeasures(y = y,   fit = fit1@fit$sigma,   naive = naive.vol), 
    .ErrorMeasures(y = y,   fit = fit2@fit$sigma,   naive = naive.vol), 
    .ErrorMeasures(y = y,   fit = fit5@fit$sigma,   naive = naive.vol))) 
print(ErrorMeas)

#mincer-zarnowitz
.MincerZarnowitz(y = y, fit = fit1@fit$sigma, msg = 'GARCH\n') #significativo, prev. distorte
.MincerZarnowitz(y = y, fit = fit2@fit$sigma, msg = 'GJR-GARCH\n') #significativo, prev. distorte
.MincerZarnowitz(y = y, fit = fit5@fit$sigma, msg = 'T-GARCH\n') #significativo, prev. distorte
# .MincerZarnowitz(y = y, fit = fit6@fit$sigma, msg = 'IGARCH\n') #significativo, prev. distorte

#diebold-mariano volatilità
h = 1
e1 = y - fit1@fit$sigma
e2 = y - fit2@fit$sigma
e5 = y - fit5@fit$sigma
# e6 = y - fit6@fit$sigma
.DieboldMariano(e1 = e1, e2 = e2, h = h, power = 1, msg = 'GARCH vs GJR-GARCH ->')
.DieboldMariano(e1 = e1, e2 = e2, h = h, power = 2, msg = 'GARCH vs GJR-GARCH ->') #non si rifiuta
.DieboldMariano(e1 = e1, e2 = e5, h = h, power = 1, msg = 'GARCH vs T-GARCH   ->')
.DieboldMariano(e1 = e1, e2 = e5, h = h, power = 2, msg = 'GARCH vs T-GARCH   ->') #tgarch migliore
# .DieboldMariano(e1 = e1, e2 = e6, h = h, power = 1, msg = 'GARCH vs IGARCH    ->')
# .DieboldMariano(e1 = e1, e2 = e6, h = h, power = 2, msg = 'GARCH vs IGARCH    ->') #garch migliore

#previsioni
H = 10
#ex-post
spec1x = getspec(fit5)
setfixed(spec1x) = as.list(coef(fit5))
forc2 = ugarchforecast(fitORspec = spec1x, n.ahead = H, data = yret, out.sample = NROW(yret) - 1500, n.roll = 0)
forc2@forecast

#ex-ante
forc1 = ugarchforecast(fitORspec = fit5, n.ahead = H, data = NULL, out.sample = 0, n.roll = 0)
forc1@forecast