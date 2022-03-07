x = c(1,2,3)
var(x)

ourvar = function(x)
{
  mean(x^2)-mean(x)^2
}

ourvar
ourvar(x)*3/2

ourvar(1:12)
var(1:12)
m1=matrix(1:9,3,3)
m1
m1
ourvar(m1)

x1=seq(-3,3,le=100)
x2=x1^2
plot(x1,x2)
plot(x1,x2,type="l")

x1=seq(-1,1,le=100)
x2=x1

f= function(x1,x2)
{
  x1^4-2*x2^2+(2*x1^2-1)*x2^2
}

f(0,0)
resouter=outer(x1,x2,f)
resouter
persp(x1,x2,resouter,ticktype="detailed")
contour(x1,x2,resouter)
