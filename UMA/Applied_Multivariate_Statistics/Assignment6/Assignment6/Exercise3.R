### Exercise 3

remove(list = ls())

euro  = read.table(file = "Europe.txt", header = T, dec =".")
attach(euro)
europe = data.frame(CPI, UNE, INP, BOP, PRC, UN)
europe = na.omit(europe)
ls(europe)

X = scale(europe, center = T, scale = T)
R = cor(europe)
E = eigen(R)$vectors

#  i)
A1hat = X%*%E[,1]%*%t(E[,1])
head(A1hat,5)
AE1 = (dim(X)[1]-1)*sum(eigen(R)$values[2:6])
AE1

#  ii)
A2hat = X%*%E[,1:2]%*%t(E[,1:2])
head(A2hat, 5)

AE2 = (dim(X)[1]-1)*sum(eigen(R)$values[3:6])
AE2

#  iii)
A3hat = X%*%E[,1:3]%*%t(E[,1:3])
head(A3hat, 5)

AE3 = (dim(X)[1]-1)*sum(eigen(R)$values[4:6])
AE3
