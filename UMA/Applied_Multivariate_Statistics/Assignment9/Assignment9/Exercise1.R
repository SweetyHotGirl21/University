### Exercise 1

remove(list = ls())

euro  = read.table(file = "Europe.txt", header = T, dec =".")
attach(euro)
europe = data.frame(euro)
europe = na.omit(europe)
Country = ï..Country
numbers = cbind(CPI, UNE, INP, BOP, PRC, UN)

# a) 
Euro = scale(numbers, center = TRUE, scale = TRUE)
EuroEuc = dist(Euro, method = "euclidean", diag = TRUE, upper = TRUE)
reshclust = hclust(EuroEuc, method = "complete")
plot(reshclust, hang = -1, labels = Country, ylab = "Euclidean Distance")
rect.hclust(reshclust, k = 5, border = c(1,2,3,4,5))

# b) 
S1 = cov(Euro)
lam1 = eigen(S1)$values[1:2]
e1 = eigen(S1)$vectors[,1:2]
Yhat = Euro%*%e1
plot(-Yhat[,1],Yhat[,2], type = "n", asp = 1, ylab = "PC2", xlab = "PC1")
text(-Yhat[,1],Yhat[,2], Country,  col = cutree(reshclust, k = 5))

arrows(0,0,-2*e1[1,1],2*e1[1,2], col = "blue", lwd = 2)
text(-2.5*e1[1,1],2.5*e1[1,2], labels = variable.names(europe[2]), col = "blue")
arrows(0,0,-2*e1[2,1],2*e1[2,2], col = "blue", lwd = 2)
text(-2.5*e1[2,1],2.5*e1[2,2], labels = variable.names(europe[3]), col = "blue")
arrows(0,0,-2*e1[3,1],2*e1[3,2], col = "blue", lwd = 2)
text(-2.5*e1[3,1],2.5*e1[3,2], labels = variable.names(europe[4]), col = "blue")
arrows(0,0,-2*e1[4,1],2*e1[4,2], col = "blue", lwd = 2)
text(-2.5*e1[4,1],2.5*e1[4,2], labels = variable.names(europe[5]), col = "blue")
arrows(0,0,-2*e1[5,1],2*e1[5,2], col = "blue", lwd = 2)
text(-2.5*e1[5,1],2.5*e1[5,2], labels = variable.names(europe[6]), col = "blue")
arrows(0,0,-2*e1[6,1],2*e1[6,2], col = "blue", lwd = 2)
text(-2.5*e1[6,1],2.5*e1[6,2], labels = variable.names(europe[7]), col = "blue")

# c)

# d)
R1 = cor(Euro)
round(R1, digits = 2)
EuroDist = as.dist(1-R1)
EuroClus = hclust(EuroDist, method = "complete")
plot(EuroClus, hang = -1, labels = variable.names(europe[2:7]), ylab = "1-R Distances")

