###Assignment 1

###Exercise 1

x1 = c(1,4,1,3,2,-6)
x2 = c(-1,5,2,4,1,-1)
x3 = c(-1,5,1,0,-1,3)

### a)
2*x1-x2+x3

### b)
###  i)
sum(x1*x2)

###  ii)
sum(x2*x3)

###  iii)
sum(x1*x3)

#x1 und x3 sind orthogonal zu einander"

### c)
x1%*%t(x2)

### d)
L1 = norm(as.matrix(x1),type="2")
L1
L2 = norm(as.matrix(x2),type="2")
L2
L3 = norm(as.matrix(x3),type="2")
L3

### d) alternativ
L1 = sqrt(sum(x1^2))
L1
L2 = sqrt(sum(x2^2))
L2
L3 = sqrt(sum(x3^2))
L3

### e) Calculate the projection of
###  i) x1 on x2
pro12 = (((t(x2)%*%x1)/L2)%*%(1/L2))%*%x2
pro12

### ii) x1 on x3
pro13 = (((t(x3)%*%x1)/L3)%*%(1/L3))%*%x3
pro13

#Mit diesem Ergebnis war zu rechnen, da diese beiden Vektoren orthogonal zueinander sind. 