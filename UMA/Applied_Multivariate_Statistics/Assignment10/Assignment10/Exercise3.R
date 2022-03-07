### Exercise 3

remove(list = ls())

students2009 = read.table("students2009.txt", header = TRUE, dec = ",")
students2009 = students2009[c(1,4,35,36)]
students2009 = na.omit(students2009)



