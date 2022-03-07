###Exercise 6

remove(list = ls())

### a)
A = matrix(c(13,-4,2,-4,13,-2,2,-2,10),3,3)
EA = eigen(A)
lamEA = EA$values

###Eigenvalue matrix
m_lamEA = matrix(c(18,0,0,0,9,0,0,0,9),3,3)
m_lamEA

###Eigenvector matrix
eiVA = EA$vectors
eiVA

###sectral decompostion
eiVA%*%m_lamEA%*%t(eiVA)
###Wie zu erwarten entspreicht das Ergebnis der sectral decomposition der Matrix.

### b)

eiEA1 = EA$vectors[,1]
eiEA2 = EA$vectors[,2]
eiEA3 = EA$vectors[,3]

###  i)
lamEA[1]*eiEA1%*%t(eiEA1)

###  ii)
lamEA[1]*eiEA1%*%t(eiEA1)+lamEA[2]*eiEA2%*%t(eiEA2)

###  iii)
lamEA[1]*eiEA1%*%t(eiEA1)+lamEA[2]*eiEA2%*%t(eiEA2)+lamEA[3]*eiEA3%*%t(eiEA3)

### c)
sqm_lamEA = sqrt(matrix(c(18,0,0,0,9,0,0,0,9),3,3))
sqm_A = eiVA%*%sqm_lamEA%*%t(eiVA)
sqm_A
