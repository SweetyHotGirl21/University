###Assignment 1

remove(list = ls())

###Exercise 6

vx = c(1,0,0,0,0,1)
x = matrix(vx,3,2)
y = c(1,3,2)

yd = x%*%(solve(t(x)%*%x))%*%t(x)%*%y
yd

