### Exercise 3

remove(list = ls())
I  = diag(1, nrow = 23, ncol = 23)
J = matrix(c(rep(1, 23*23)), nrow = 23, ncol = 23)
n = 1/230
H = I-n*J

# a)
#  i)
pref = read.table(file = "Preferences.txt", header = T)
Pref =  cbind(Name = pref[,1],pref[,2:21])
Preferences = as.matrix(Pref[,2:21])
X = scale(Preferences, center = T, scale = F)
Q = X%*%t(X)
Qlam = eigen(Q)$values[1:2]
QLam = diag(sqrt(Qlam), nrow = 2, ncol = 2)
Qe = eigen(Q)$vectors[,1:2]
Yhat = Qe%*%QLam
head(Yhat,4)

#  ii)
S = cov(Preferences)
E = eigen(S)$vectors
Yhat = X%*%E
head(Yhat[,1:2],4)

# b)
X = scale(Preferences, center = T, scale = F)
S = cov(X)
Lam = eigen(S)$values[1:2]
E = eigen(S)$vectors[,1:2]
Yhat = X%*%E
plot(Yhat, type = "n", asp = 1, xlab = "PC1", ylab = "PC2")
text(Yhat[,1],Yhat[,2], pref[,1])

# c)
E_1 = E[,1]
E_2 = E[,2]
lqa = sqrt(E_1^2+E_2^2)
ra = order(lqa, decreasing = T)
arrows(0,0,2.5*E_1[ra[1:4]],2.5*E_2[ra[1:4]], col="red")
text(3*E_1[ra[1:4]],3*E_2[ra[1:4]], labels = c("Q4","Q19","Q1","Q18"), col = "red")        

# d) 
colMeans(Preferences)
Conformist =  c(1,1,-1,-1,-1,1,-1,1,-1,-1,1,-1,1,1,1,-1,-1,1,1,-1)
Individualist = (-1)*Conformist
means = colMeans(Preferences, na.rm = F, dims = 1)
XC = Conformist-means
XI = Individualist-means
YhatC = XC%*%E
YhatI = XI%*%E

text(YhatC[1,1],YhatC[1,2], "conformist", col = "green")
text(YhatI[1,1],YhatI[1,2], "individualist", col = "green")
