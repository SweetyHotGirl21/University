###Exercise 3

remove(list = ls())

x1 = seq(-1,1,le=40)
x2=x1

# i)

A1=matrix(c(1,0,0,1),2,2)
f= function(v1,v2)
{
  A1[1,1]*v1^2+A1[2,2]*v2^2+2*A1[1,2]*v1*v2
}
resouter=outer(x1,x2,f)
persp(x1,x2,asp = 1,resouter,ticktype="detailed", zlab = "z")
contour(x1,x2,resouter,asp = 1)

# ii)

A2=matrix(c(1,0,0,2),2,2)
f= function(v1,v2)
{
  A2[1,1]*v1^2+A2[2,2]*v2^2+2*A2[1,2]*v1*v2
}
resouter=outer(x1,x2,f)
persp(x1,x2,asp = 1,resouter,ticktype="detailed", zlab = "z")
contour(x1,x2,resouter,asp=1)

###  iii)

A3=matrix(c(1,0,0,0.5),2,2)
f= function(v1,v2)
{
  A3[1,1]*v1^2+A3[2,2]*v2^2+2*A3[1,2]*v1*v2
}
resouter=outer(x1,x2,f)
persp(x1,x2,asp = 1,resouter,ticktype="detailed", zlab = "z")
contour(x1,x2,resouter,asp=1)

###  iv)

A4=matrix(c(1,0.5,0.5,1),2,2)
f= function(v1,v2)
{
  A4[1,1]*v1^2+A4[2,2]*v2^2+2*A4[1,2]*v1*v2
}
resouter=outer(x1,x2,f)
persp(x1,x2,asp = 1,resouter,ticktype="detailed", zlab = "z")
contour(x1,x2,resouter,asp=1)


###  v)

A5=matrix(c(1,0.8,0.8,1),2,2)
f= function(v1,v2)
{
  A5[1,1]*v1^2+A5[2,2]*v2^2+2*A5[1,2]*v1*v2
}
resouter=outer(x1,x2,f)
persp(x1,x2,asp = 1,resouter,ticktype="detailed", zlab = "z")
contour(x1,x2,resouter,asp=1)

###  vi)

A6=matrix(c(1,-0.5,-0.5,1),2,2)
f= function(v1,v2)
{
  A6[1,1]*v1^2+A6[2,2]*v2^2+2*A6[1,2]*v1*v2
}
resouter=outer(x1,x2,f)
persp(x1,x2,asp = 1,resouter,ticktype="detailed", zlab = "z")
contour(x1,x2,resouter,asp=1)

###  vii)

A7 = solve(A5)
f= function(v1,v2)
{
  A7[1,1]*v1^2+A7[2,2]*v2^2+2*A7[1,2]*v1*v2
}
resouter=outer(x1,x2,f)
persp(x1,x2,asp = 1,resouter,ticktype="detailed", zlab = "z")
contour(x1,x2,resouter,asp=1)

###  viii)

A8 = solve(A6)
f= function(v1,v2)
{
  A8[1,1]*v1^2+A8[2,2]*v2^2+2*A8[1,2]*v1*v2
}
resouter=outer(x1,x2,f)
persp(x1,x2,asp = 1,resouter,ticktype="detailed", zlab = "z")
contour(x1,x2,resouter,asp=1)


