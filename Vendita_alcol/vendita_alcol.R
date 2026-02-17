#estraggo i dati
alcol <- read.csv("./dataset.csv", sep=",")
dates <- as.Date(alcol$observation_date, format="%Y-%m-%d")
dollars <- alcol$Spesa

#mostro i valori della serie temporale
plot(dates, dollars, type="l", ylab="", xlab="t", main="Dollari spesi per acquistare alcol (in milioni)")

#disegno i correlogrammi
par(mfrow=c(2,1))
acf(dollars, lag.max=36, main="ACF campionaria vendita alcol")
pacf(dollars, lag.max=36, main="PACF campionaria vendita alcol")

#disegno il monthplot
monthplot(dollars)

#disegno il seasonal plot
dollars_ts <- ts(dollars, start=c(1992,1), frequency=12)

y1 <- window(dollars_ts, start=c(1992,1), end=c(1992,12))
y2 <- window(dollars_ts, start=c(1997,1), end=c(1997,12))
y3 <- window(dollars_ts, start=c(2002,1), end=c(2002,12))
y4 <- window(dollars_ts, start=c(2007,1), end=c(2007,12))
y5 <- window(dollars_ts, start=c(2012,1), end=c(2012,12))
y6 <- window(dollars_ts, start=c(2017,1), end=c(2017,12))
y7 <- window(dollars_ts, start=c(2022,1), end=c(2022,12))

range(dollars_ts)

x <- seq(1, 12, 1)
matplot(x, y1, type="l", ylim=c(0,8000), ylab="", xlab="Mese")
lines(x, y2, type="l", lty=2, col="red")
lines(x, y3, type="l", lty=2, col="blue", lwd=3)
lines(x, y4, type="l", lty=2, col="green", lwd=3)
lines(x, y5, type="l", lty=2, col="pink", lwd=2)
lines(x, y6, type="l", lty=2, col="brown", lwd=2)
lines(x, y7, type="l", lty=2, col="orange", lwd=2)

#disegno il lag plot
lag.plot(dollars_ts, set.lags=1:12, type="p", do.lines=FALSE)

#detrendizzo la serie temporale
dollars_detr <- diff(dollars, lag=1, diff=1)

#destagionalizzo la serie temporale
dollars_detr_dest <- diff(dollars_detr, lag=12, diff=1)

plot(dates[-(1:13)], dollars_detr_dest, type="l", ylab="", xlab="t", main="Vendita alcol detrendizzata e destagionalizzata")

#disegno i correlogrammi
par(mfrow=c(2,1))
acf(dollars_detr_dest, lag.max=36, main="ACF campionaria vendita alcol detrendizzata e destagionalizzata")
pacf(dollars_detr_dest, lag.max=36, main="PACF campionaria vendita alcol detrendizzata e destagionalizzata")

#elimino trend e stagionalità della serie usando un modello moltiplicativo
dollars_detr_dest <- decompose(dollars_ts, type="multiplicative")
plot(dollars_detr_dest)

#disegno i correlogrammi della serie temporale
serie_pulita <- na.omit(dollars_detr_dest$random)
par(mfrow=c(2,1))
acf(serie_pulita, lag.max=36, main="ACF campionaria vendita di alcol detrendizzata e destagionalizzata")
pacf(serie_pulita, lag.max=36, main="PACF campionaria vendita di alcol detrendizzata e destagionalizzata")

#eseguo test di Ljung-Box
Box.test(serie_pulita, lag=20, type="Ljung-Box")