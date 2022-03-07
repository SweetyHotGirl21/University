### Exercise 6

remove(list = ls())

students  = read.table(file = "students2008.txt", header = T, dec =",")
attach(students)
heightweight=data.frame(height,weight)
heightweight=na.omit(heightweight)
attach(heightweight)
m1=mean(height)
m2=mean(weight)
c2=qchisq(0.95,2)
S=cov(heightweight)
Sinv=solve(S)
a11=Sinv[1,1]
a12=Sinv[1,2]
a22=Sinv[2,2]
x1=seq(150,210,le=1000)
x2=seq(40,110,le=1000)
f=function(x1,x2)
{
  a11*(x1-m1)^2+a22*(x2-m2)^2+2*a12*(x1-m1)*(x2-m2)
}
z=outer(x1,x2,f)

# a)

contour(x1,x2,z,asp=1,levels=c2,drawlabels=FALSE,lwd=2,
        cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
        xlab="height",ylab="weight",main="Students 2008")
reseigen=eigen(S)
lambda=reseigen$values
E=reseigen$vectors
lambda1=lambda[1]
lambda2=lambda[2]
e1=E[,1]
e2=E[,2]
arrows(m1,m2,sqrt(c2*lambda1)*e1[1]+m1,sqrt(c2*lambda1)*e1[2]+m2,lwd=2, col = "red")
arrows(m1,m2,sqrt(c2*lambda2)*e2[1]+m1,sqrt(c2*lambda2)*e2[2]+m2,lwd=2, col = "red")
arrows(m1,m2,-sqrt(c2*lambda1)*e1[1]+m1,-sqrt(c2*lambda1)*e1[2]+m2,lwd=2, col = "red")
arrows(m1,m2,-sqrt(c2*lambda2)*e2[1]+m1,-sqrt(c2*lambda2)*e2[2]+m2,lwd=2, col = "red")

points(height,weight,pch=16)

# ii)
X = scale(heightweight, TRUE, FALSE)
S = cov(heightweight)
Ee = eigen(S)
S = matrix(c(Ee$values[1],0,0,Ee$values[2]),2,2)
Sinv = solve(S)
lam1 = eigen(S)$values[1]
lam2 = eigen(S)$values[2]

c2=qchisq(0.95,2)

x1 = seq(-40,40, le = 1000)
x2 = seq(-30,30, le = 1000)

f=function(x1,x2)
{
  1/(lam1)*(x1)^2+1/lam2*(x2)^2
}
z=outer(x1,x2,f)

contour(x1,x2,z,asp=1,levels=c2,drawlabels=FALSE,lwd=2,
        cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
        xlab="height",ylab="weight",main="Students 2008")

reseigen=eigen(S)

lambda=reseigen$values
E=reseigen$vectors
lambda1=lambda[1]
lambda2=lambda[2]
e1=E[,1]
e2=E[,2]
m1 = 0
m2 = 0
arrows(m1,m2,sqrt(c2*lambda1)*e1[1]+m1,sqrt(c2*lambda1)*e1[2]+m2,lwd=2, col = "red")
arrows(m1,m2,sqrt(c2*lambda2)*e2[1]+m1,sqrt(c2*lambda2)*e2[2]+m2,lwd=2, col = "red")
arrows(m1,m2,-sqrt(c2*lambda1)*e1[1]+m1,sqrt(c2*lambda1)*e1[2]+m2,lwd=2, col = "red")
arrows(m1,m2,sqrt(c2*lambda2)*e2[1]+m1,-sqrt(c2*lambda2)*e2[2]+m2,lwd=2, col = "red")

y1 = X%*%Ee$vectors[,1]
y2 = X%*%Ee$vectors[,2]

Y = cbind(y1,y2)
points(Y, pch = 16)

boxplot(c(y1),c(y2))

plot(y1,height, pch = 16, asp = 1)
plot(y2,height, pch = 16, asp = 1)
plot(y1, weight, pch = 16, asp = 1)
plot(y2, weight, pch = 16, asp = 1)

# b) 
round(Ee$vectors, digits = 4)
round(Ee$values, digits = 4)

round(cor(y1,height), digits = 4)
round(cor(y1,weight), digits = 4)
round(cor(y2,height), digits = 4)
round(cor(y2,weight), digits = 4)

round(Ee$values[1]/sum(Ee$values), digits = 4)
round(sum(Ee$values)/sum(Ee$values), digits = 4)