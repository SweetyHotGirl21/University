###Assignment 1

remove(list = ls())

###Exercise 4

mv1 = c(3,2,1,0,2,1,-1,4,3)
m1 = matrix(vm1,3,3)
m1
mv2 = c(1,2,-1,5,1,3)
m2 = matrix(mv2,2,3, byrow = TRUE)
m2
v1 = c(1,4,1)
v2 = c(-1,5,2)

###  i)
m1%*%v1

###  ii)
m2%*%v1

###  iii)
t(v1)%*%m1%*%v1

###  iv)
m2%*%m1

###  v)
t(m1)%*%m1

###  vi)
t(m2)%*%m2

###  vii)
m1%*%t(m1)

###  viii)
#Worng dimensions for a matrix multiplication


