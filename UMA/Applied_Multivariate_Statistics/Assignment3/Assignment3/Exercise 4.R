### Exercise 4

remove(list = ls())

e = c(1,0,-1,2)
e
s = c(1,0.2,0.4,-0.5,0.2,2,0.8,0,0.4,0.8,2,0,-0.5,0,0,1)
S = matrix(s,4,4)
S

x1 = c(0,0,0,0)
x2 = c(1,1,1,1)
x3 = c(1,0,1,0)

# a)
#  i)

denx1 = (1/(((2*pi)^(4/2))*sqrt(det(S))))*exp((-0.5)*(t(x1-e)%*%solve(S)%*%(x1-e)))
denx1

#  ii)

denx2 = (1/(((2*pi)^(4/2))*sqrt(det(S))))*exp((-0.5)*(t(x2-e)%*%solve(S)%*%(x2-e)))
denx2

#  iii)

denx3 = (1/(((2*pi)^(4/2))*sqrt(det(S))))*exp((-0.5)*(t(x3-e)%*%solve(S)%*%(x3-e)))
denx3

# b) 

#  i)

(1/(((2*pi)^(4/2))*sqrt(det(S))))*exp((-0.5)*(qchisq(0.95,4)))

#  ii)

(1/(((2*pi)^(4/2))*sqrt(det(S))))*exp((-0.5)*(qchisq(0.9,4)))

#  iii)

(1/(((2*pi)^(4/2))*sqrt(det(S))))*exp((-0.5)*(qchisq(0.8,4)))

