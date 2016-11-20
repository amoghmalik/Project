

mydata = read.csv("Traffic_data_orig.csv")
secretMessage = c(0,1,1,1,0,1,0,0,0,1,1,0,1,0,0,0,0,1,1,0,1,0,0,1,0,1,1,1,0,0,1,1,0,1,1,0,1,0,0,1,0,1,1,1,0,0,1,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,1,0,1,1,0,0,1,0,1,0,1,1,0,0,0,1,1,0,1,1,1,0,0,1,0,0,1,1,0,0,1,0,1,0,1,1,1,0,1,0,0,0,1,1,0,1,1,0,1,0,1,1,0,0,1,0,1,0,1,1,1,0,0,1,1,0,1,1,1,0,0,1,1,0,1,1,0,0,0,0,1,0,1,1,0,0,1,1,1,0,1,1,0,0,1,0,1)

covertData = mydata[1:(length(secretMessage)+1),]
zeroDelay = 0.25
oneDelay = 0.75

#instanciate a vector to store inter packet delays for the PMF histogram
ipDelays = numeric(nrow(covertData))

for (i in 2:length(secretMessage)){
    #Insertion of the secret message using packet delays
    if(secretMessage[i] == 0)
        covertData[i,2] = zeroDelay
    else
        covertData[i,2] = oneDelay
                                        #if(i != 1)
    covertData[i,2] = covertData[i,2] + covertData[i-1,2]

    #Store inter-delay value
    ipDelays[i-1]= covertData[i,2] - covertData[i-1,2]
}

proba = c( mean(ipDelays == zeroDelay), mean(ipDelays == oneDelay))
names(proba)= c(0.25,0.75)
barplot(proba,main= "PMF for X",xlab="k",ylab="P(X=k)")



