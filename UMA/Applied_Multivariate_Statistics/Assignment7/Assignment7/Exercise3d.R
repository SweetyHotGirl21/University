# d)
euro  = read.table(file = "Europe.txt", header = T, dec =".")
attach(euro)
europe = data.frame(euro)
europe = na.omit(europe)
Country = ï..Country
numbers = cbind(CPI, UNE, INP, BOP, PRC, UN)
S = cov(numbers)

R = cor(numbers)

Ehat = eigen(R)$vectors
Lamhat = eigen(R)$values
Lam = diag(x = c(Lamhat),nrow = 6, ncol = 6)
round(Lam, digits = 4)
Lamhatsqu = sqrt(Lam)
round(Lamhatsqu, digits = 4)
ltil = Ehat%*%Lamhatsqu
Ltil = round(ltil, digits = 4)

c = diag(1-((Ltil[,1])^2+(Ltil[,2])^2),6,6)
round(R-Ltil[,1:2]%*%t(Ltil[,1:2])-c, digits = 4)

