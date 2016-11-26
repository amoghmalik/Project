old.par <- par(mfrow = c(1, 2))
# this line allow multiple plots for this problem
# (http://www.dummies.com/programming/r/how-to-put-multiple-plots-on-a-single-page-in-r/)

# Implementation part, Question 1 & 2
overtData = read.csv("Traffic_data_orig.csv")

# Enable simulations parameters
simulationNb = 1000
bufSize = 20
messageSizes = c(16, 32)
bufInitState = c(2, 6, 10, 14, 18)


# We want to do the simulation for an exponential distribution and a uniform one
# Here if distIsExp is TRUE we will use the exponential distribution 
# And if not the uniform one
for (distIsExp in c(TRUE, FALSE)) {
    for (messageSize in messageSizes) {
        # Instanciate vectors to store the number of overflows or underflows
        overflows = numeric(length(bufInitState))
        underflows = numeric(length(bufInitState))
        for (initBuf in bufInitState) {
            # Compute index to store overflows and underflows
            flowInd = match(initBuf, bufInitState)
            for (sim in 1:simulationNb) {
                # 1 Generate random bit pattern
                message = sample(c(0, 1), messageSize, replace = TRUE)

                # 2 Generate source (src) sequence of packets' generation times.
                srcPktsNeeded = messageSize - initBuf + 1
                # Instanciate a flag for when src generate zero packets.
                emptySrc = FALSE
                if (srcPktsNeeded < 0) {
                    srcPktsNeeded = 0
                    emptySrc = TRUE
                }
                if (distIsExp == TRUE)
                    src = rexp(srcPktsNeeded, 1)
                else
                    src = runif(srcPktsNeeded, 0, 1)
                # If there is more than 1 value in src 
                # We want to cumulate them to create timeframe.
                if (length(src) > 1) {
                    for (i in 2:length(src)) {
                        src[i] = src[i] + src[i - 1]
                    }
                }

                # 3 Instanciate the initial state of the buffer and src
                curBuf = initBuf
                curSrc = 1

                # 4 instanciate a variable storing the time when the next packet is sent
                timeCurPkt = 0
                # Generate a pool of values following the well known source distribution
                poolDelays = rexp(messageSize * 10, 1)
                # We need to find the index of the median 
                # In the sorted ip-delays of the pool.
                sortedPoolDelays = sort(poolDelays)
                med = median(sortedPoolDelays)

                # 5 for each secret message bit
                for (i in 2:(messageSize + 1)) {
                    # Insertion of the ith message bit using packet delays
                    if (message[i - 1] == 0) {
                        # Here we are assuming that Bob knows an ip-delay 
                            # Equal to median will be a 0.
                        timeCurPkt = timeCurPkt + sample(sortedPoolDelays[sortedPoolDelays <= med], 1)
                    }
                    else {
                        timeCurPkt = timeCurPkt + sample(sortedPoolDelays[sortedPoolDelays > med], 1)
                    }

                    # Update buffer state
                    while (curSrc <= length(src)
                        && !emptySrc
                        && (src[curSrc] < timeCurPkt)) {
                        # Source generate an additional packet
                        # In the buffer before sender use one
                        curSrc = curSrc + 1
                        curBuf = curBuf + 1
                    }
                    # Check for overflow
                    if (curBuf >= 21) {
                        overflows[flowInd] = overflows[flowInd] + 1
                        break
                    }
                    # Now send the packet
                    curBuf = curBuf - 1
                    # Check for underflow
                    if (curBuf < 0) {
                        underflows[flowInd] = underflows[flowInd] + 1
                        break
                    }
                }
            }
        }
        if (distIsExp == TRUE) {
            cat("\n")
            cat("FOR EXPONENTIAL DISTRIBUTION: ")
            cat("\n")
        }
        else {
            cat("\n")
            cat("FOR UNIFORM DISTRIBUTION: ")
            cat("\n")
        }
        overflows
        cat('probability of overflow with message size of ')
        cat(messageSize)
        cat(" is: ")
        cat("\n")
        cat((overflows / simulationNb))
        cat(" for 2,6,10,14 and 18 initial packets in the buffer, respectively.")
        cat("\n")
        cat("\n")
        cat("probability of underflow with message size of ")
        cat(messageSize)
        cat(" is: ")
        cat("\n")
        cat((underflows / simulationNb))
        cat(" for 2,6,10,14 and 18 initial packets in the buffer, respectively.")
        cat("\n")
    }
}
