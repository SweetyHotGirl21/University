###Assignment 1

remove(list = ls())

###Exercise 4

mv1 = c(3,2,1,0,2,1,-1,4,3)
m1 = matrix(mv1,3,3)
mv2 = c(1,2,-1,5,1,3)
m2 = matrix(mv2,2,3, byrow = TRUE)
v1 = c(1,4,1)
v2 = c(-1,5,2)

###  i)
m1%*%v1
#Diese Matrix ist nicht symmertisch

###  ii)
m2%*%v1
#Diese Matrix ist nicht symmertisch

###  iii)
t(v1)%*%m1%*%v1
#Diese Matrix ist nicht symmertisch

###  iv)
m2%*%m1
#Diese Matrix ist nicht symmertisch

###  v)
t(m1)%*%m1
#Diese Matrix ist symmertisch

###  vi)
t(m2)%*%m2
#Diese Matrix ist symmertisch

###  vii)
m1%*%t(m1)
#Diese Matrix ist symmertisch

###  viii)
m2%*%t(m2)
#Diese Matrix ist symmertisch



