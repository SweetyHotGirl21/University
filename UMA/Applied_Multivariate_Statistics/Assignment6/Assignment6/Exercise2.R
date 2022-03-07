### Exercise 2

remove(list = ls())

euro  = read.table(file = "Europe.txt", header = T, dec =".")
attach(euro)
europe = data.frame(euro)
europe = na.omit(europe)
ls(europe)
Country = ï..Country
numbers = cbind(CPI, UNE, INP, BOP, PRC, UN)

# a) 
R = cor(numbers)
E = eigen(R)$vectors

X = scale(numbers, center = T, scale = T)
Yhat = X%*%E
y1hat = Yhat[,1]*(-1)
y2hat = Yhat[,2]

plot(y1hat, y2hat, type = "n", asp = 1, xlab = "PC1", ylab = "PC2", xlim = c(-4,3), ylim = c(-2, 4))
text(y1hat, y2hat, Country)

arrows(0,0,-E[,1]*1.5,E[,2]*1.5, lwd = 2, col = "blue")
text(-E[,1]*2, E[,2]*2, c("CPI","UNE","INP","BOP","PRC","UN"), col = "blue")

# b)
Rich = data.frame(ï..Country = "Rich", CPI = mean(CPI), UNE = mean(UNE), INP = mean(INP), BOP = max(BOP)*3, PRC = mean(PRC), UN = mean(UN))
rbind.data.frame(europe, Rich)

XRich = c(0,0,0,(3*max(BOP)-mean(BOP))/sd(BOP),0,0)
YhatRich = XRich%*%E
plot(y1hat, y2hat, type = "n", asp = 1, xlab = "PC1", ylab = "PC2",ylim = c(-5,4))
text(y1hat,y2hat, Country)
text(-YhatRich[1,1],YhatRich[1,2], "Rich")
arrows(0,0,-E[,1]*2,E[,2]*2, col = "blue", lwd = 2)
text(-E[,1]*2.2,E[,2]*2.2, c("CPI","UNE","INP","BOP","PRC","UN"), col = "blue")

