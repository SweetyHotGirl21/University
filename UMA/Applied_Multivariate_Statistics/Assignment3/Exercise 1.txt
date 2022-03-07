### Exercise 1

remove(list = ls())

A = matrix(c(13,-4,2,-4,13,-2,2,-2,10),3,3)

# a) Correlation matrix C

C = cov2cor(A)
C

# b)

CE = eigen(C)
CE

### Die Eigenwerte und die Eigenvektoren der Korrelationsmatrix sind nicht die gleichen, wie die der Kovarianzmatrix A

