### Exercise 7

remove(list = ls())

getwd()

students  = read.table(file = "students2008.txt", header = T, dec =",")
attach(students)
hw = data.frame(height,weight)
heigthweight = na.omit(hw)
detach(students)
attach(heigthweight)

# a)

X = cbind(height,weight)

m1 = mean(height)
m2 = mean(weight)
m = c(m1,m2)
s = cov(X)
S = solve(s)

x1 = seq(140,220, le = 1000)
x2 = seq(40,110, le = 1000)

f = function(v1,v2)
{
  S[1,1]*(v1-m1)^2+S[2,2]*(v2-m2)^2+2*S[2,1]*(v1-m1)*(v2-m2)
}

resouter = outer(x1,x2,f)
contour(x1,x2, resouter, levels = qchisq(0.95,2), asp = 1, drawlabels = FALSE, main = "Students 2018", xlab = "height", ylab = "weight")

# b)

qchisq(0.95,2)
points(height,weight, pch =  16)
points(m1,m2,pch=16,col = "red")

c = sqrt(qchisq(0.95,2))
arrows(m1,m2,m1+eigen(S)$vectors[1,1]*c/sqrt(eigen(S)$values[1]),
       m2+eigen(S)$vectors[2,1]*c/sqrt(eigen(S)$values[1]))
arrows(m1,m2,m1-eigen(S)$vectors[1,2]*c/sqrt(eigen(S)$values[2]),
       m2-eigen(S)$vectors[2,2]*c/sqrt(eigen(S)$values[2]))

e11 = eigen(S)$vectors[1,1]*c/sqrt(eigen(S)$values[1])
e12 = eigen(S)$vectors[2,1]*c/sqrt(eigen(S)$values[1])
e21 = eigen(S)$vectors[1,2]*c/sqrt(eigen(S)$values[2])
e22 = eigen(S)$vectors[2,2]*c/sqrt(eigen(S)$values[2])

L1 = sqrt(e11^2+e12^2)
L1
L2 = sqrt(e21^2+e22^2)
L2

# c)

?mahalanobis

dis = mahalanobis(heigthweight,m,solve(S))
dm = dim(heigthweight)[1]
c = sqrt(qchisq(0.95,2))
sum((sqrt(dis))<c)/dm

