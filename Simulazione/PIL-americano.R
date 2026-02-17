pil.us <- read.csv("GDPC1.csv", sep=",")
PIL <- pil.us$GDPC1
xt <- ts(PIL, start=c(1947,1), freq=4)

plot(xt, col="black", ylab="", xlab="t", main="Andamento serie storica PIL americano")

coefficients <- rep(1/5, 5)
ma <- filter(xt, coefficients, method="convolution", sides=2, circular=FALSE)
lines(ma, col="green", lwd=2)
legend("topleft", legend=c("Dati originali", "Media mobile"), col=c("black", "green"), lty=1, lwd=c(1,2), bty="n")

coefficients <- c(1/8, 1/4, 1/4, 1/4, 1/8)
mac <- filter(xt, coefficients, method="convolution", sides=2, circular=FALSE)
lines(mac, col="red", lwd=3)
legend("topleft", legend=c("Dati originali", "Media mobile 1", "Media mobile 2"), col=c("black", "green", "red"), lty=1, lwd=c(1,2,3), bty="n")

detrendx <- xt - mac
plot(detrendx, ylab="", xlab="t", main="Serie del PIL US detrendizzata")

detrendxx <- xt / mac
plot(detrendx, ylab="", xlab="t", main="Serie del PIL US detrendizzata")

d.pil <- decompose(xt, type="additive")
plot(d.pil)

d.pil <- decompose(xt, type="multiplicative")
plot(d.pil)

dxt <- diff(xt, lag=1, differences=1)
ddxt <- diff(dxt, lag=4, differences=1)

par(mfrow=c(2,1))
plot(dxt, ylab="", xlab="t", main="Serie del PIL detrendizzata")
plot(ddxt, ylab="", xlab="t", main="Serie del PIL detrendizzata e destagionalizzata")

ddxt.mensile <- diff(dxt, lag=12, differences=1)
plot(ddxt.mensile, ylab="", xlab="t", main="Serie del PIL detrendizzata e con destagionalizzazione mensile")
