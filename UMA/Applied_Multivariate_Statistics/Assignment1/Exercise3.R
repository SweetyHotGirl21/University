###Assignment 1

remove(list = ls())

###Exercise 3

### a)
x = 0
y = 0
x1 = c(2,1)
y1 = c(3,0)

plot(x,y,xlim=c(0,4),ylim=c(-0.2,1.2),type="n",axes=FALSE,frame.plot=FALSE,ann=FALSE,asp=1)
arrows(0,0,2,1,col = 4)
arrows(0,0,3,0,col = 2)
arrows(0,-0.1,2,-0.1)

lx1 = sqrt(sum(x1^2))
ly1 = sqrt(sum(y1^2))
projxy = (((t(y1)%*%x1)/ly1)%*%(1/ly1))%*%y1
projxy
#     [,1] [,2]
#[1,]  2.0  0.0

arrows(2,1,projxy[1,1],projxy[1,2], lty = 2, code = 0)
arrows(2,1,2,0, lty = 2, code = 0)
text(2.2,1.1,"x")
text(3.2,0,"y")

### b)
x = 0
y = 0
x1 = c(2,1)
y1 = c(3,1)

plot(x,y,xlim=c(0,4),ylim=c(-0.2,1.2),type="n",axes=FALSE,frame.plot=FALSE,ann=FALSE,asp=1)
arrows(0,0,2,1,col = 4)
arrows(0,0,3,1,col = 2)
arrows(0,-0.1,2,-0.1)

lx1 = sqrt(sum(x1^2))
ly1 = sqrt(sum(y1^2))
projxy = (((t(y1)%*%x1)/ly1)%*%(1/ly1))%*%y1
projxy
#     [,1] [,2]
#[1,]  2.1  0.7
arrows(2,1,projxy[1,1],projxy[1,2], lty = 2, code = 0)

text(2.2,1.1,"x")
text(3.2,1,"y")

### c) 
?plot
#the y/x aspect ratio: If asp is a finite positive value then the window is setup so that
#one data unit in the x direction is equal in length to asp * one data unit in the y direction.


