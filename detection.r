set.seed(124)
norm <- rnorm(30, 0)
norm2 <- rnorm(30, 1)
qqplot(norm, norm2)


norm <- rnorm(100, 0)
norm2 <- rnorm(100, 1)
qqplot(norm, norm2)


norm <- rnorm(1000, 0)
norm2 <- rnorm(1000, 1)
qqplot(norm, norm2)

#Observation: There are many more points on the graph <-  much denser, much more accurate. 
#Nevertheless the general trend is the same 
#There are also outliers

#3
norm <- rnorm(100, 0)
norm2 <- rnorm(100, 5, 3)
qqplot(norm, norm2)

#Obeservations



#4
qqplot(rexp(100, 1),rexp(100, 1))
qqplot(rexp(1000, 1),rexp(1000, 1))


#5
qqplot(rexp(100, 1),rnorm(100, 0))
qqplot(rexp(500, 1),rnorm(500, 0))


#6


