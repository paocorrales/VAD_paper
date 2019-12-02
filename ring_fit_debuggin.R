fitted <- (cbind(1, cos(azimuth * pi/180), 
                 sin(azimuth * pi/180))[!nas, , drop = FALSE] %*% fit$coefficients)[, 1]

x11()
plot(azimuth, ring)
points(azimuth[!nas][outliers], ring[!nas][outliers], col = "red")
lines(azimuth[!nas], fit$fitted.values)

x11()
plot(azimuth[!nas], fit$residuals)
points(azimuth[!nas][outliers], fit$residuals[outliers], col = "red")
