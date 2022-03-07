### Händisch!!!
### Nicht abgeben!!!

m = matrix(c(3,-sqrt(2),-sqrt(2),2),2,2)
em = eigen(m)
em
1/(sqrt(3))
em1 = em$vectors[,1]
em1
4*em1%*%t(em1)
t(em1)%*%(em1)
em1%*%t(em1)

m2 = matrix(c(1,-5,-5,1),2,2)
em2 = eigen(m2)

(em2$vectors[,1])%*%em2$vectors[,2]

v11 = c(1/sqrt(3),-sqrt(2)/sqrt(3))
v22 = c(sqrt(2)/sqrt(3),1/sqrt(3))
t(v11)%*%v22
4*v11
v11
v22
