white_noise <- arima.sim(list(order=c(0,0,0)), n = 200)

plot(white_noise, ylab="", xlab="t", main="Simulazione processo white noise")

par(mfrow=c(2,1))
acf(white_noise, lag.max=20, main="ACF campionaria (h=0, 1,..., 20)")
pacf(white_noise, lag.max=20, main="PACF campionaria (h=0, 1,..., 20)")


ar <- arima.sim(list(order=c(1,0,0), ar=0.2), n = 200)
plot(ar, type="l", main="Processo AR(1)")

ma_y <- arima.sim(list(order=c(0,0,1), ma=0.5), n=200)
ma_z <- arima.sim(list(order=c(0,0,1), ma=-0.5), n=200)
par(mfrow=c(2,1))
plot(ma_y, type="l", main="Processo MA(1)")
plot(ma_z, type="l", main="Processo MA(1)")

ma_y1 <- arima.sim(list(order=c(0,0,1), ma=0.5), n=200)
ma_y2 <- arima.sim(list(order=c(0,0,1), ma=0.5), n=1000)
ma_y3 <- arima.sim(list(order=c(0,0,1), ma=0.5), n=10000)

par(mfrow=c(3,1))
acf(ma_y1, lag.max=20, main="ACF campionaria con n=200")
acf(ma_y2, lag.max=20, main="ACF campionaria con n=1000")
acf(ma_y3, lag.max=20, main="ACF campionaria con n=10000")

ma_z1 <- arima.sim(list(order=c(0,0,1), ma=-0.5), n=200)
ma_z2 <- arima.sim(list(order=c(0,0,1), ma=-0.5), n=1000)
ma_z3 <- arima.sim(list(order=c(0,0,1), ma=-0.5), n=10000)

par(mfrow=c(3,1))
acf(ma_z1, lag.max=20, main="ACF campionaria con n=200")
acf(ma_z2, lag.max=20, main="ACF campionaria con n=1000")
acf(ma_z3, lag.max=20, main="ACF campionaria con n=10000")

exact_acf_y <- ARMAacf(ar=0, ma=0.5)
exact_acf_z <- ARMAacf(ar=0, ma=-0.5)
exact_acf_y
exact_acf_z

exact_acf_x <- ARMAacf(ar=0, ma=c(0.2, -0.15, 0.2), lag.max=10)
exact_acf_x