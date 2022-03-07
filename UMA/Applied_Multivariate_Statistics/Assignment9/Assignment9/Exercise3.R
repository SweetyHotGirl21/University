### Exercise 3

remove(list = ls())

pers3 = read.table(file = "Personality.txt", header = T, dec =".")

# a)
Pers3 = scale(pers3, scale = FALSE, center = TRUE)
R = cor(pers3)
PersDelt = as.dist(1-R)
Persreshc = hclust(PersDelt, method = "average")
plot(Persreshc,  hang = -1, labels = pers3$Name, ylab = "1 -R Distance")
rect.hclust(Persreshc, k = 4, border = c(1,2,3,4) )
Group = sort(cutree(Persreshc, k = 4))
Group[1:12]    
# Cluster 1
Group[13:14]
# Cluster 2
Group[15:28]
# Cluster 3
Group[29:32]
# Cluster 4

# b)
X3 = R
Q = X3%*%t(X3)
QE = eigen(Q)$vectors
Qlam = eigen(Q)$values
QLam = diag(Qlam[1:2])
QYhat = QE[,1:2]%*%sqrt(QLam)
plot(QYhat[,1],QYhat[,2], asp = 1, type = "n")
text(QYhat[,1],QYhat[,2], variable.names(pers3), col = cutree(Persreshc, k = 4))

# c)
Chol = c(0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0)
San = c(0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0)
CSan = 2*San
Mel = c(1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0)
CMel = 3*Mel
phle = c(rep(1,32))
Phle = phle-(Chol+San+Mel)
CPhle = 4*Phle
Color = CPhle+Chol+CSan+CMel
plot(QYhat[,1],QYhat[,2], asp = 1, type = "n")
text(QYhat[,1],QYhat[,2], variable.names(pers3), col = Color)

