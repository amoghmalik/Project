old.par <- par(mfrow = c(1, 2)) #this line and par(old.par) allow multiple plots for this problem
							  #(http://www.dummies.com/programming/r/how-to-put-multiple-plots-on-a-single-page-in-r/)
#Question 1

overtData = read.csv("Traffic_data_orig.csv")
secretMessage = c(0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1,
	 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0,
	  0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 
	  0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0,
	   0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1)

covertData = overtData[1:(length(secretMessage)+1),]
zeroDelay = 0.25
oneDelay = 0.75

for (i in 2:(length(secretMessage)+1)){
    #Insertion of the secret message using packet delays
    if(secretMessage[i-1] == 0)
        covertData[i,2] = zeroDelay
    else
        covertData[i,2] = oneDelay
    #if(i != 1)
    covertData[i,2] = covertData[i,2] + covertData[i-1,2]   
}

#Question 2

#Instanciate vectors to store inter packet delays for PMF histograms
ipDelaysCovert = numeric(nrow(covertData)-1)
ipDelaysOvert = numeric(nrow(overtData)-1)

#Here we assume delays will be between 0 and 1 sec
possibleInterDelays = seq.int(0, 1, 0.01)

#Compute inter-delay values for overt and covert channels
for (i in 2:nrow(overtData)) {
	ipDelaysOvert[i - 1] = round(overtData[i, 2] - overtData[i - 1, 2], 2)
	if (i <= nrow(covertData))
		ipDelaysCovert[i - 1] = round(covertData[i, 2] - covertData[i - 1, 2], 2)
}

#Instanciate vectors to store inter-delay probabilities
probaCovert = numeric(length(possibleInterDelays))
probaOvert = numeric(length(possibleInterDelays))
for (i in 1:length(possibleInterDelays)){
	#Store inter-delay probabilities
	probaOvert[i] = mean(ipDelaysOvert == possibleInterDelays[i])
	probaCovert[i] = mean(ipDelaysCovert == possibleInterDelays[i])
}

#Plot histograms for the inter-delay channels PMF's
names(probaCovert) = possibleInterDelays
barplot(probaCovert, main = "PMF for X, inter-delays' RV of the covert channel", xlab = "k", ylab = "P(X=k)")
names(probaOvert) = possibleInterDelays
barplot(probaOvert, main = "PMF for X, inter-delays' RV of the overt channel", xlab = "k", ylab = "P(X=k)")





