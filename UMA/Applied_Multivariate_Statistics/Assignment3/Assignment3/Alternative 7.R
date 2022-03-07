### Exercise 7

remove(list = ls())

getwd()

students  = read.table(file = "students2008.txt", header = T, dec =",")
attach(students)
heightweight = data.frame(height,weight)
heigthweight = na.omit(heightweight)
detach(students)
attach(heigthweight)

x1 = seq(140,220, le = 1000)
x2 = seq(40,110, le = 1000)

X = cbind(height,weight)

m1 = mean(height)
m2 = mean(weight)
m = c(m1,m2)
s = cov(X)
S = solve(s)

COR = cov2cor(S)
dens = (1/((2*pi)*sqrt(S11*S22)*sqrt(1-COR12^2)))*exp(-0.5*qchisq(0.95,2))

f= function(v1,v2)
{
  (1/((2*pi)*sqrt(S11*S22)*sqrt(1-COR12^2)))*exp((-1/(2*(1-COR12^2)))*(((v1-m1)^2/S11)
  -2*COR12*((v1-m1)/sqrt(S11))*((v2-m2)/sqrt(S22))+((v2-m2)^2)/S22))
}

