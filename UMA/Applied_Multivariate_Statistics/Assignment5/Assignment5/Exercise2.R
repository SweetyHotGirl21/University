### Exercise 2

remove(list = ls())

library(rJava)
library(iplots)

getwd()



euro  = read.table(file = "Europe.txt", header = T, dec =".")
attach(euro)
europe = data.frame(euro)
europe = na.omit(europe)
ls(europe)
Country = europe$ï..Country
CPI = europe$CPI
UNE = c(europe$UNE)
INP = c(europe$INP)
BOP = c(europe$BOP)
PRC = c(europe$PRC)
UN = c(europe$UN)

numbers = cbind(CPI,UNE,INP,BOP,PRC,UN)
S = cov(numbers)
r = round(cor(numbers), digits = 4)

# a) 
pairs(numbers)

reseigen = eigen(r)
lambda = round(reseigen$values, digits = 4)
plot(lambda, type = "l", main = "Scree-Plot", xlab = "# component")

# b)
round(r, digits = 4)

# c)
E = round(reseigen$vectors, digits = 4)
X = scale(numbers, center = TRUE, scale = TRUE)
Yhat = (X)%*%E
y1hat = Yhat[,1]
y2hat = Yhat[,2]
ihist(y1hat)
ihist(y2hat)
ihist(CPI)
ihist(UNE)
ihist(INP)
ihist(BOP)
ihist(PRC)
ihist(UN)


# d)
E = round(reseigen$vectors, digits = 4)
E
lambda
round(cumsum(lambda)/sum(diag(r)),4)
e1 = E[,1]
e2 = E[,2]
e3 = E[,3]
e4 = E[,4]
e5 = E[,5]
e6 = E[,6]



### e)
lambda1 = lambda[1]
lambda2 = lambda[2]
rec1 = round(lambda1/sum(lambda), digits = 4)
paste("The first principal component explains", rec1*100, "% of the total variation. It is mostly driven by CPI and PRC. It is bigger for countriese with a low Consumer Price Index and high final consumption expenditure")
rec2 = round(lambda2/sum(lambda), digits = 4)
paste("The second  principal component explains", rec2*100, "% of the total variation. It is mostly driven by INP, BOP and UN. It is bigger for Countries with a low industiral Production, low Balances of Payments and high changes in Unemployment")

### f)
y1hatr = round(Yhat[,1], digits = 2)
y2hatr = round(Yhat[,2], digits = 2)
y12hat = data.frame(Country,y1hatr ,y2hatr, CPI,UNE,INP,BOP,PRC,UN)
y12hat

###  i) 
paste("Denmark, Ireland, France, Italy, Cyprus, Finland, UK")
paste("Diese Länder haben die Eigenschaften, dass sie eine geringe Infaltion haben und einen hohen Konsum. Außerdem haben sie ein geringe Industirelle Produktion und eine gerninge Balance of Payments. Weiter eint sie eie starke Änderungen in der Arbeitslosigkeit.")

###  ii)
paste("Bulgaria, Greece, Spain, Portugal, Slovenia, Portugal")
paste("Diese Länder haben eine hohe Infaltion und geringen privaten Konsum. Die Industielle Produtkoin ist gering, wie auch die Balance of Payments. Außerdem ist die Arbeitslosenquote stark schwankend. Dies ist die Gruppe der Südeuropäischenländer, welche die Finazkrise stark getroffen hat.")

###  iii)
paste("Czech Republik, Estonia, Latvia, Lithuania, Hungary, Poland, Romania, Slovakia")
paste("Hohe Infaltion und niedriger privater Konsum. Allerdings sind industrielle Produktion und die Balance of Payments hoch. Die Schwankungen in der Arebitslosigkeit sind gering. Dies ist die Gruppe der osteuropäischen Länder.")

###  iv)
paste("Belgium, Germany, Luxembourg, Malta, Netherlands, Austria, Malta, Sweden")
paste("Die Länder eint eine gerninge Inflation und hoher privater Konsum, eine ausgeprägte industirelle Produktion, eine hohe Balance of Payments und gernige Schwankungen in der Arbeitslosigkeit. Es ist die Gruppe der nord- und mitteleuropäischen Länder die Volkswirtschaftlich sehr gut darstehen.")


plot(y1hat,y2hat, type ="n")
text(y1hat,y2hat)
