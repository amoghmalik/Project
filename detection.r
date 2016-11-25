


#TO DO 
# 45 Degree line
# Graph Titles DONE
# Comments  DONE
# Bigger sample, gives better


set.seed(124)
norm <- rnorm(30, 0)
norm2 <- rnorm(30, 1)
qqplot(norm, norm2, xlab = "")


norm <- rnorm(100, 0)
norm2 <- rnorm(100, 1)
qqplot(norm, norm2)

norm <- rnorm(1000, 0)
norm2 <- rnorm(1000, 1)
qqplot(norm, norm2)
abline(a=0,b=1)
#Observation: We can see from the graph that as the size of n increases so does the accuracy,
# hence the larger the n, the more similar the distributions are to each other. 


#3
norm <- rnorm(100, 0)
norm2 <- rnorm(100, 5, 3)
qqplot(norm, norm2)
abline(a=0,b=1)
#Obeservations
#The two distributions are not equal to each other, 
#as by looking at the 45 degree line we can see that the line is "shifted" to the right, 
# which means that .....
# the shape of the line is correct, just not the values


#4
qqplot(rexp(100, 1),rexp(100, 1))
qqplot(rexp(1000, 1),rexp(1000, 1))
abline(a=0,b=1)

#5
qqplot(rexp(100, 1),rnorm(100, 0))
qqplot(rexp(500, 1),rnorm(500, 0))
abline(a=0,b=1)

#6
#Code in file: design1.r
#Observation:
#As we can see from the plot the graph does not resemble a straight line at all, and 
# also is not anywhere near the 45 degree line, which tells us that the two distributions
# were not similar at all and hence the convesation was subjected to an intruder Eve,
#there was no proper security. 

#7
#Code in file: design34.r
#This is much batter that the method used in Q6, but still we can see that it has its limiatioans. 
#The graph is not a perfect straight line going through the 45 degree line, which is why we can conclude 
#that this is also not the most secure method. 

#8
#Code in file: design5.r
#This is the best result we obtained in comparison to the prevoius graph.
#The graph now actually covers the 45 degree line, which proves that the two distributions are very similar. 
#Hence in this case it would be hard for Eve to overhear the convesation between Alice and Bob. 


