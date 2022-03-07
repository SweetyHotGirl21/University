### Exercise 2

remove(list = ls())

library(tools)
library(HSAUR)
data("voting")

# a)
DelDis = voting

I  = diag(1, nrow = 15, ncol = 15)
J = matrix(c(rep(1, 15*15)), nrow = 15, ncol = 15)
n = 1/15
H = I-n*J
DelDis2 = DelDis^2
B = -0.5*H%*%DelDis2%*%H
round(eigen(B)$values, digits = 4)  
### B has negativ eigenvalues, so it is not nonnegativ definite. Therfor it is not Euclidean.

# b)
Q = -0.5*H%*%DelDis2%*%H
Qe = eigen(Q)$vectors[,1:2]
Qlam = eigen(Q)$values[1:2]
QLam = diag(sqrt(Qlam), nrow = 2, ncol = 2)
Yhat = Qe%*%QLam
CM = c("Hunt(R)", "Sandman(R)", "Howard(D)", "Thompson(D)",
                "Freylinghuysen(R)", "Forsythe(R)", "Widnall(R)", 
                "Roe(D)", "Heltoski(D)", "Rodino(D)", "Minish(D)", 
                "Rinaldo(R)", "Maraziti(R)", "Daniels(D)", "Patten(D)")
plot(Yhat, type = "n", asp = 1, xlab = "Coordinate 1", ylab = "Coordinate 2", xlim = c(-13,7), ylim = c(-6,8))
text(Yhat[,1],Yhat[,2], CM)

# c)
data.frame("C1" = cmdscale(voting, k = 2)[,1],"Yhat1" = Yhat[,1],"C2" = cmdscale(voting, k = 2)[,2],"Yhat2" = Yhat[,2])
### These are the same results.
