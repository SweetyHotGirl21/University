###Assignment 1

remove(list = ls())

###Exercise 5

av = c(2,1,3,4,3,8,-2,2,0,-4,5,1,-1,3,4,1)
A = matrix(av,4,4)

### a)
det(A)

#Die Determinante(A) ist !=0. Daraus ist zu folgern, dass die Matrix vollen Rang hat und die Spaltenvektoren linear unabhängig sind. 

### b)
mv2 = c(1,2,-1,5,1,3)
m2 = matrix(mv2,2,3, byrow = TRUE)

dm2 = det(t(m2)%*%m2)
round(dm2)

#Die Derterminate(t(m2)%*%m2)) = 0. Somit hat die Matrix keinen vollen Rang und linear abhängige Spaltenvektoren. Es exsistiert keine Inverse zu dieser Matrix

### c)
iA = solve(A)
round(iA, digits = 4)


