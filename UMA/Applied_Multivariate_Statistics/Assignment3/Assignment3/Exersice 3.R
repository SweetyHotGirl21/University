### Exercise 3

remove(list = ls())

m = c(1,0,-1,2)
m
s = c(1,0.2,0.4,-0.5,0.2,2,0.8,0,0.4,0.8,2,0,-0.5,0,0,1)
S = matrix(s,4,4)
S

COR = cov2cor(S)
round(COR, digits = 4)

### X3 und X4 sind unkorreliert, Die beiden paare X1 X2, X2 X3 sind korreliert.  

# b)

#  i)

m1 = m[3]
S1 = S[3,3]

paste("X3 ~ N(",m1,",",S1,")")

