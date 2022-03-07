### Exercise 5   

remove(list = ls())

m = c(1,0.5)
m1 = 1
m2 = 0.5
s = matrix(c(1,0.8,0.8,1),2,2)
S = solve(s)

# a) 

x1 = seq(-3,4,le=100)
x2 = x1

f= function(v1,v2)
{
 S[1,1]*(v1-m1)^2+S[2,2]*(v2-m2)^2+2*S[2,1]*(v1-m1)*(v2-m2)
}

c = qchisq(0.95,2)
z = outer(x1,x2,f)
contour(x1,x2, z, levels = c, asp = 1, drawlabels = FALSE)


# b) 

dens = (2*pi)^-1*det(s)^-0.5*exp(-0.5*qchisq(0.95,2))
dens1 = round(dens, digits = 5)
paste("Ervery point on this contour exhibit is lying on on the density value", dens1)

# c) 

eigen(s)

L1 = sqrt(eigen(s)$values[1])*sqrt(qchisq(0.95,2))
L1
L2 = sqrt(eigen(s)$values[2])*sqrt(qchisq(0.95,2))
L2

