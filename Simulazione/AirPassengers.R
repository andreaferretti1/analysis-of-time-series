data("AirPassengers")
plot(AirPassengers, ylab="", xlab="t", main="AirPassengers")

monthplot(AirPassengers)

lag.plot(AirPassengers, set.lags=1:12, type="p", do.lines=FALSE)

y1 <- window(AirPassengers, start=c(1949,1), end=c(1949,12))
y2 <- window(AirPassengers, start=c(1951,1), end=c(1951,12))
y3 <- window(AirPassengers, start=c(1954,1), end=c(1954,12))
y4 <- window(AirPassengers, start=c(1956,1), end=c(1956,12))
y5 <- window(AirPassengers, start=c(1958,1), end=c(1958,12))
y6 <- window(AirPassengers, start=c(1960,1), end=c(1960,12))

range(AirPassengers)

x <- seq(1, 12, 1)
matplot(x, y1, type="l", ylim=c(100,650), ylab="", xlab="Mese")
lines(x, y2, type="l", lty=2, col="red")
lines(x, y3, type="l", lty=2, col="blue", lwd=3)
lines(x, y4, type="l", lty=2, col="green", lwd=3)
lines(x, y5, type="l", lty=2, col="pink", lwd=2)
lines(x, y6, type="l", lty=2, col="brown", lwd=2)

yt <- diff(AirPassengers, lag=1, differences=1)
zt <- diff(yt, lag=12, differences=1)

par(mfrow=c(2,1))
plot(zt, ylab="", xlab="t", main="Serie AirPassengers differenziata")
acf(zt, lag.max=20, main="ACF serie AirPassengers differenziata")

