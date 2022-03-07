### Exercise 1

remove(list = ls())

SwissLabor=read.table(file="SwissLabor.txt",header=T)
attach(SwissLabor)

summary(SwissLabor)

plot(participation~age,ylevels=2:1,main="Participation and Age")
plot(participation~education,ylevels=2:1, main="Participation and Education")

plot(participation~income,ylevels=2:1, main="Participation and Income")
plot(participation~foreign,ylevels=2:1, main="Participation and Foreign")

boxplot(age~youngkids,main="Age and Young Kids", xlab="Number of Young Kids",ylab="Age")

boxplot(education~foreign,main="Education and Foreign", ylab="Years of Education")

mean(age[youngkids==1&education>=10])
mean(age[youngkids==1&education<10])
mean(age[youngkids==2&education<10])

age2=age^2
reslogit=glm(participation~income+age+age2+education+youngkids+oldkids+foreign,family=binomial)
summary(reslogit)

resprobit=glm(participation~income+age+age2+education+youngkids+oldkids+foreign,family=binomial(link="probit"))
summary(resprobit)
