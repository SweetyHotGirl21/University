###Assignment 1

remove(list = ls())

###Exercise 3

# a)
x = 0
y = 0
plot(x,y,xlim=c(0,4),ylim=c(-0.2,1.2),type="n",axes=FALSE,frame.plot=FALSE,ann=FALSE,asp=1)
arrows(0,0,2,1,col = 4)
arrows(0,0,3,0,col = 2)
arrows(0,-0.1,2,-0.1)
arrows(2,1,2,0, lty = 2, code = 0)
text(2.2,1.1,"x")
text(3.2,0,"y")
