### Exercise 4

remove(list = ls())

appro = read.table(file = "Approx2dim.txt", header = T, dec =".")
attach(appro)
x1 = ï..x1
Approx2 = data.frame(x1,x2)
Approx2 = na.omit(Approx2)

plot(x1,x2, asp = 1, xlim = c(-4,3), ylim = c(-2, 4), main = "Least Squares Regression and Eigenvector Projection")
abline(h = 0, v = 0)

reslm = lm(x2 ~ x1 - 1)
x2hatlm = reslm$fitted.values

abline(reslm, col= "blue")
points(x1,x2hatlm, pch = 16, col = "blue")

S = cov(Approx2)
E = eigen(S)$vectors
X = scale(Approx2, center = F, scale = T)

Ahat = X%*%E[,1]%*%t(E[,1])
reslm2 = lm(Ahat[,2] ~ Ahat[,1] -1)
x2hatlm2 = reslm2$fitted.values

abline(reslm2, col = "red", lwd = 2)
points(Ahat[,1], x2hatlm2, pch = 16, col ="red")

#Approximation Error for OLS = sum of residuals
sum((reslm$residuals)^2)

# Approximation Error
AE = (dim(X)[1]-1)*sum(eigen(S)$values[2])
AE
 

