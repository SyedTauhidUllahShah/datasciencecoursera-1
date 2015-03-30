# Perform a Simple Good-Turing analysis, using R.
#
# Input is a data frame with two items per line: frequency, r, and non-zero
# frequencies of frequencies, Nr.
#
# Output lines contain the frequency r, and the SGT r*.
#
# To get the probability for an object seen r>=1 times, divide r* by N.

nrzest <- function(r, nr){
     d <- c(1, diff(r))
     dr <- c(0.5 * (d[-1] + d[-length(d)]), d[length(d)])
     return(nr/dr)
}

rstest <- function(r, coef) {
     return(r * (1 + 1/r)^(1 + coef[2]))
}

SimpleGoodTuring <- function(xr, xnr) {
     # xr is a vector of frequencies
     # xnr is a vector of freqeuncies of frequencies
     xN <- sum(xr*xnr)
     
     # make averaging transform
     xnrz <- nrzest(xr, xnr)
     
     # get Linear Good-Turing estimate
     xf <- lsfit(log(xr), log(xnrz))
     xcoef <- xf$coef
     xrst <- rstest(xr, xcoef)
     xrstrel <- xrst/xr
     
     # get Turing estimate
     xrtry <- xr == c(xr[-1]-1, 0)
     xrstarel <- rep(0, length(xr))
     xrstarel[xrtry] <- (xr[xrtry]+1)/xr[xrtry]*c(xnr[-1],0)[xrtry]/xnr[xrtry]
     
     # make switch from Turing to LGT estimates
     tursd <- rep(1, length(xr))
     for(i in 1:length(xr)) {
          if(xrtry[i]) {
               tursd[i] <-(i+1)/xnr[i]*sqrt(xnr[i+1]*(1+xnr[i+1]/xnr[i]))
          }
     }
     xrstcmbrel <- rep(0, length(xr))
     useturing <- TRUE
     for(r in 1:length(xr)) {
          if(!useturing) xrstcmbrel[r] <- xrstrel[r]
          else if(abs(xrstrel-xrstarel)[r] * r/tursd[r] > 1.65) {
               xrstcmbrel[r] <- xrstarel[r]
          }
          else {
               useturing <- FALSE
               xrstcmbrel[r] <- xrstrel[r]
          }
     }
     
     # renormalize the probabilities for observed objects
     sumpraw <- sum(xrstcmbrel*xr*xnr/xN)
     xrstcmbrel <- xrstcmbrel*(1-xnr[1]/xN)/sumpraw
     result <- data.frame()
     result[1,1] = as.integer(0)
     result[1,2] = xnr[1]/xN
     result[1,3] = result[1,2]
     for(i in 1:length(xr)){
          result[i+1,1] <- xr[i]
          result[i+1,2] <- xr[i]*xrstcmbrel[i]
          result[i+1,3] <- result[i+1,2]/xN
     }
     colnames(result) <- c("r", "r_star", "Probability")
     return(result)
}
