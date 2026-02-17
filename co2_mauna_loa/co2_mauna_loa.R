#estraggo i dati
co2_data <- read.csv("./co2_mm_mlo.csv", sep=",", comment.char="#")

co2_values <- co2_data$average

starting_year <- co2_data$year[1]
starting_month <- co2_data$month[1]

co2_ts <- ts(co2_values, start=c(starting_year, starting_month), frequency=12)
plot(co2_ts, type="l", ylab="", xlab="t", main="Concentrazione CO2")

#disegno i correlogrammi
par(mfrow=c(2,1))
acf(co2_ts, lag.max=36, main="ACF campionaria concentrazione CO2")
pacf(co2_ts, lag.max=36, main="PACF campionaria concentrazione CO2")

#disegno il monthplot
monthplot(co2_ts)

#disegno il seasonal plot
y1 <- window(co2_ts, start=c(1960,1), end=c(1960,12))
y2 <- window(co2_ts, start=c(1970,1), end=c(1970,12))
y3 <- window(co2_ts, start=c(1980,1), end=c(1980,12))
y4 <- window(co2_ts, start=c(1990,1), end=c(1990,12))
y5 <- window(co2_ts, start=c(2000,1), end=c(2000,12))
y6 <- window(co2_ts, start=c(2010,1), end=c(2010,12))
y7 <- window(co2_ts, start=c(2020,1), end=c(2020,12))

range(co2_ts)

x <- seq(1, 12, 1)
matplot(x, y1, type="l", ylim=c(270,450), ylab="", xlab="Mese")
lines(x, y2, type="l", lty=2, col="red")
lines(x, y3, type="l", lty=2, col="blue", lwd=3)
lines(x, y4, type="l", lty=2, col="green", lwd=3)
lines(x, y5, type="l", lty=2, col="pink", lwd=2)
lines(x, y6, type="l", lty=2, col="brown", lwd=2)
lines(x, y7, type="l", lty=2, col="orange", lwd=2)

#disegno il lag plot
lag.plot(co2_ts, set.lags=1:12, type="p", do.lines=FALSE)

#detrendizzo la serie temporale e disegno il nuovo lag plot
co2_detr <- diff(co2_ts, lag=1, differences=1)

lag.plot(co2_detr, set.lags=1:12, type="p", do.lines=FALSE)

#destagionalizzo la serie temporale detrendizzata
co2_detr_dest <- diff(co2_detr, lag=12, differences=1)

plot(co2_detr_dest)

par(mfrow=c(2,1))
acf(co2_detr_dest, lag.max=36, main="ACF campionaria concentrazione CO2 detrendizzata e destagionalizzata")
pacf(co2_detr_dest, lag.max=36, main="PACF campionaria concentrazione CO2 detrendizzata e destagionalizzata")

#eseguo il test di Ljung-box
Box.test(co2_detr_dest, lag=20, type="Ljung-Box")
