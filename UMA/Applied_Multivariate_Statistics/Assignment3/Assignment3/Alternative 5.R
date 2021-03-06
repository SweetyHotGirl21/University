### Exercise 5   

remove(list = ls())


m = c(1,0.5)
m1 = 1
m2 = 0.5
S = matrix(c(1,0.8,0.8,1),2,2)


S11 = S[1,1]
S22 = S[2,2]

COR12 = COR[1,2]

# a) 

#  i)
x1 = seq(-3,4,le=100)
x2 = x1

dens = sqrt((1/((2*pi)*sqrt(1-COR12^2)))*exp((-1/(2*(1-COR12^2)))*qchisq(0.95,2)))


####sli 74

f= function(v1,v2)
{
  (1/((2*pi)*sqrt(S11*S22)*sqrt(1-COR12^2)))*exp((-1/(2*(1-COR12^2)))*(((v1-m1)^2/S11)
  -2*COR12*((v1-m1)/sqrt(S11))*((v2-m2)/sqrt(S22))+((v2-m2)^2)/S22))
}

z = outer(x1,x2,f)
z = outer(x1,x2,f)
persp(x1,x2,z,main="",cex.lab=1.5,theta=30,phi=20,r=50,d=0.1,expand=0.5,
      ltheta=90,lphi=180,shade=0.75,ticktype="detailed",nticks=5,
      xlim=c(-3,3),ylim=c(-3,3),zlim=c(0,0.2))
contour(x1,x2, resouter, levels = dens, asp = 1, drawlabels = FALSE)




####################

# b) 
dens1 = round(dens, digits = 5)
paste("Ervery point on this contour exhibit is lying on on the density value", dens1)

# c) ### Folie 55 mean centeres Elipse


A = S

f= function(v1,v2)
{
  A[1,1]*(v1-1)^2+A[2,2]*(v2-0.5)^2+2*A[1,2]*(v1-1)*(v2-0.5)
}

resouter=outer(x1,x2,f)
contour(x1 ,x2 ,resouter,levels = dens, asp = 1, drawlabels = FALSE)

E=eigen(A)
E

###Diese Werte stimmen mit jenen überein, welche mit Hand berechnet wurden.

e10 = E$vectors[,1]
e11 = (sqrt(dens)/sqrt(9))*e10
arrows(0,0,e11[1],e11[2],col = "black")
text(e11[1]+0.3,e11[2],"x")

e20 = E$vectors[,2]
e21 = (sqrt(2)/sqrt(1))*e20
arrows(0,0,e21[1],e21[2],col = "black")