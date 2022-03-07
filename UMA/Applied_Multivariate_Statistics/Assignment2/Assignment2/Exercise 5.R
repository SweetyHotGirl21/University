### Exercise 5

remove(list = ls())

x1 = seq(-2,2,le=100)
x2 = x1
A = matrix(c(5,4,4,5),2,2)

f= function(v1,v2)
{
  A[1,1]*v1^2+A[2,2]*v2^2+2*A[1,2]*v1*v2
}

resouter=outer(x1,x2,f)
contour(x1 ,x2 ,resouter,levels = 2, asp = 1, drawlabels = FALSE, xlim=c(-2,2), ylim = c(-2,2))

E=eigen(A)
E

###Diese Werte stimmen mit jenen überein, welche mit Hand berechnet wurden.

e10 = E$vectors[,1]
e11 = (sqrt(2)/sqrt(9))*e10
arrows(0,0,e11[1],e11[2],col = "black")
text(e11[1]+0.3,e11[2],"x")

e20 = E$vectors[,2]
e21 = (sqrt(2)/sqrt(1))*e20
arrows(0,0,e21[1],e21[2],col = "black")
text(e21[1]-0.3,e21[2],"y")

le11 = sqrt(sum(e11^2))
paste("Die Länge des Eigenvektors x beträgt:", round(le11, digits = 6))

le21 = sqrt(sum(e21^2))
paste("Die Länge des Eigenvektors y beträgt:", round(le21, digits = 6))

