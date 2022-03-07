### check

### Assignment 3

## Exercise 4

remove(list = ls())

mu=c(1,0,-1,2)
covv=c(1,0.2,0.4,-0.5,0.2,2,0.8,0,0.4,0.8,2,0,-0.5,0,0,1)
covm=matrix(covv,4,4)

# a)

f=function(x1,x2,x3,x4)
{
  (1/(((2*pi)^2)*sqrt(det(covm))))*exp(-0.5*(t(c(x1,x2,x3,x4)-mu)%*%solve(covm)%*%(c(x1,x2,x3,x4)-mu)))
}

# (i)

f(0,0,0,0)

# (ii)

f(1,1,1,1)

#(iii)

f(1,0,1,0)

