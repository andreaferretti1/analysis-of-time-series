#estraggo i dati dal file
ind_prod <- read.csv("./INDPRO.csv", sep=",")
ind_prod_values <- ind_prod$INDPRO
dates <- as.Date(ind_prod$observation_date, format="%Y-%m-%d")

#mostro i valori della serie temporale
plot(dates, ind_prod_values, type="l", xlab="t", ylab="", main="Indice di produzione dal 1919 al 2025")

#disegno il correlogramma
par(mfrow=c(2,1))
acf(ind_prod_values, lag.max=200, main="ACF campionaria produzione industriale")
pacf(ind_prod_values, lag.max=200, main="PACF campionaria produzione industriale")

#traccio il monthplot
monthplot(ind_prod_values)

#disegno il seasonal plot
ind_prod_ts <- ts(ind_prod_values, start=c(1919,1), frequency=12)

y1 <- window(ind_prod_ts, start=c(1920,1), end=c(1920,12))
y2 <- window(ind_prod_ts, start=c(1930,1), end=c(1930,12))
y3 <- window(ind_prod_ts, start=c(1940,1), end=c(1940,12))
y4 <- window(ind_prod_ts, start=c(1950,1), end=c(1950,12))
y5 <- window(ind_prod_ts, start=c(1960,1), end=c(1960,12))
y6 <- window(ind_prod_ts, start=c(1970,1), end=c(1970,12))
y7 <- window(ind_prod_ts, start=c(1980,1), end=c(1980,12))
y8 <- window(ind_prod_ts, start=c(1990,1), end=c(1990,12))
y9 <- window(ind_prod_ts, start=c(2000,1), end=c(2000,12))
y10 <- window(ind_prod_ts, start=c(2010,1), end=c(2010,12))

range(ind_prod_ts)

x <- seq(1, 12, 1)
matplot(x, y1, type="l", ylim=c(0,110), ylab="", xlab="Mese")
lines(x, y2, type="l", lty=2, col="red")
lines(x, y3, type="l", lty=2, col="blue", lwd=3)
lines(x, y4, type="l", lty=2, col="green", lwd=3)
lines(x, y5, type="l", lty=2, col="pink", lwd=2)
lines(x, y6, type="l", lty=2, col="brown", lwd=2)
lines(x, y7, type="l", lty=2, col="orange", lwd=2)
lines(x, y8, type="l", lty=2, col="yellow", lwd=2)
lines(x, y9, type="l", lty=2, col="purple", lwd=2)
lines(x, y10, type="l", lty=2, col="skyblue", lwd=2)

#disegno il lag plot
lag.plot(ind_prod_ts, set.lags=1:12, type="p", do.lines=FALSE)

#detrendizzo la serie temporale
detr_ind_prod_values <- diff(ind_prod_values, lag=1, diff=1)
plot(dates[-1], detr_ind_prod_values, type="l", xlab="t", ylab="", main="Indice di produzione dal 1919 al 2025 detrendizzato")

#disegno il lag plot
lag.plot(detr_ind_prod_values, set.lags=1:12, type="p", do.lines=FALSE)

#disegno i correlogrammi della serie temporale
par(mfrow=c(2,1))
acf(detr_ind_prod_values, lag.max=200, main="ACF campionaria produzione industriale detrendizzata")
pacf(detr_ind_prod_values, lag.max=200, main="PACF campionaria produzione industriale detrendizzata")

#eseguo il test di Ljung-Box
Box.test(detr_ind_prod_values, lag=20, type="Ljung-Box")

#elimino trend e stagionalità della serie usando un modello additivo
detr_ind_prod_values <- decompose(ind_prod_ts, type="multiplicative")
plot(detr_ind_prod_values)

#disegno i correlogrammi della serie temporale
serie_pulita <- na.omit(detr_ind_prod_values$random)
par(mfrow=c(2,1))
acf(serie_pulita, lag.max=120, main="ACF campionaria produzione industriale detrendizzata")
pacf(serie_pulita, lag.max=120, main="PACF campionaria produzione industriale detrendizzata")

#eseguo test di Ljung-Box
Box.test(serie_pulita, lag=20, type="Ljung-Box")
