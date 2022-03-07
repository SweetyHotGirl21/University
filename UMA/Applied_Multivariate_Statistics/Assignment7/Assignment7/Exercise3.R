### Exercise3

remove(list = ls())

euro  = read.table(file = "Europe.txt", header = T, dec =".")
attach(euro)
europe = data.frame(euro)
europe = na.omit(europe)
Country = ï..Country
numbers = cbind(CPI, UNE, INP, BOP, PRC, UN)
S = cov(numbers)

# a)
R = cor(numbers)

Ehat = eigen(R)$vectors
Lamhat = eigen(R)$values
Lam = diag(x = c(Lamhat),nrow = 6, ncol = 6)
round(Lam, digits = 4)
Lamhatsqu = sqrt(Lam)
round(Lamhatsqu, digits = 4)
ltil = Ehat%*%Lamhatsqu
Ltil = round(ltil, digits = 4)
Ltil
pcfa1 = data.frame(Variable = names(europe)[2:7], Ltil[,1:2], Communalities = ((Ltil[,1])^2+(Ltil[,2])^2), SpeVar1 = 1-((Ltil[,1])^2+(Ltil[,2])^2))
PCFA1 = format(pcfa1, digits = 4)
PCFA1
Prop = c(((sum(Ltil[,1]^2))/6),((sum(Ltil[,1]^2))/6)+(sum(Ltil[,2]^2))/6)
round(Prop, digits = 4)


# b)
Lstar = round(varimax(Ltil[,1:2])$loadings[1:6,1:2], digits = 4)
Rotma = varimax(Ltil[,1:2])$rotmat
Rotma
pcfa2 = data.frame(Variable = names(europe)[2:7], Lstar[,1:2], Communalities = ((Lstar[,1])^2+(Lstar[,2])^2), SpeVar = 1-((Lstar[,1])^2+(Lstar[,2])^2))
PCFA2 = format(pcfa2, digits = 4)
PCFA2
Prop2 = c(((sum(Lstar[,1]^2))/6),((sum(Lstar[,1]^2))/6)+(sum(Lstar[,2]^2))/6)
round(Prop2, digits = 4)
### Name Factor 1: Demand Surplus
### Name Factor 2: Business Cycle

plot(PCFA2$X1,PCFA2$X2, type = "n", asp = 1, ylim = c(-1.5,1.5), xlim = c(-1.5,1.5), xlab = "l1", ylab = "l2")
text(Lstar[,1],Lstar[,2], PCFA2$Variable)
text(Ltil[,1],Ltil[,2], PCFA1$Variable)
arrows(0,0,Rotma[1,1],Rotma[2,1])
arrows(0,0,Rotma[1,2],Rotma[2,2])
arrows(0,0,-Rotma[1,2],-Rotma[1,1])
arrows(0,0,-Rotma[1.1],-Rotma[2,1])



# c)
round(varimax(Ltil)$rotmat, digits = 4)
round(eigen(S)$vectors, digits = 4)
# They are not the same!


