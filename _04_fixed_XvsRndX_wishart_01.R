

options(stringsAsFactors=FALSE, width=350)

library(mvtnorm)

N <- 20

xbool_save_file <- TRUE



set.seed(777) ### try different seeds to see how different slope pred variance is between fixed and random inputs


b <- c(1, -1)

xSig <- diag(0.5, 2) + 0.5

###### look at Cov mx
xSig

xmu <- c(0, 0)

######### X is fixed -- not subject to sampling variation
X <- rmvnorm(N, xmu, xSig)

f.true <- X %*% b

sig <- 20


nn <- 300000

mxbhats <- matrix(NA, nn, length(b))

for(iinn in 1:nn) {
    
    y <- f.true + rnorm(N, 0, sig)
    
    b.hat <- solve(crossprod(X)) %*% (t(X) %*% y)
    
    mxbhats[ iinn, ] <- b.hat
    
}

######## true SEs
sqrt( diag(solve( N * xSig )) ) * sig





xxx <- apply( t(mxbhats) - b, 1, function(x) { return( ( mean( x^2 ) ) ) } )

##### agrees with actual sim error

##### sim var bhat
cat(
"Under fixed X, this is our simulated pred error of bhat: ",
xxx,
"\n"
)
#### hist(mxbhats[ , 1])

####### var using fixed X
cat(
"Under fixed X, this is our theoretical (using true resid variance) pred error of bhat: ",
diag(solve( crossprod(X) )) * sig^2,
"\n"
)
## apply(mxbhats, 2, sd)





if(xbool_save_file) {
    png(file.path("~", "Desktop", "distOfBhats_Xfixed_01.png"), width=1800, height=1000, pointsize=24)
}
par(mfrow=c(1, 2))
hist(mxbhats[ ,1], cex=1.4, lwd=2, xlim=c(-40, 40), main=paste0("Sim b1 with fixed X -- sim pred var = ", round(xxx[1], 2)))
abline(v=b[1], col="#00FF00", lwd=4)
hist(mxbhats[ ,2], cex=1.4, lwd=2, xlim=c(-40, 40), main=paste0("Sim b2 with fixed X -- sim pred var = ", round(xxx[2], 2)))
abline(v=b[2], col="#00FF00", lwd=4)
if(xbool_save_file) { dev.off() }



cat("\n")


##############################


N <- 20

sig <- 20

nn <- 300000

mxbhats <- matrix(NA, nn, length(b))
arinvXX <- array(NA, c(length(b), length(b), nn))

for(iinn in 1:nn) {
    
    X <- rmvnorm(N, xmu, xSig) #### X is NOT fixed -- but rather subject to sampling variation

    f.true <- X %*% b
    
    y <- f.true + rnorm(N, 0, sig)
    
    invXX <- solve(crossprod(X))
    arinvXX[ , , iinn] <- invXX
    
    b.hat <- invXX %*% (t(X) %*% y)
    
    mxbhats[ iinn, ] <- b.hat
    
}



xxx2 <- apply( t(mxbhats) - b, 1, function(x) { return( ( mean( x^2 ) ) ) } )

apply(arinvXX, c(1,2), mean) * sig^2

##### var bhat
##### sim var bhat
cat(
"Under random X, this is our simulated pred error of bhat: ",
xxx2,
"\n"
)

######### the center of the inverse Wishart distribution:
xmeanInvWishart <- solve( xSig ) / (N - length(b) - 1)
cat(
"Under random X, this is our theoretical (using true resid variance) pred error of bhat: ",
diag(sig^2 * xmeanInvWishart),
"\n",
"note that we use the expectation of the inverse Wishart for this.",
"\n"
)



if(xbool_save_file) {
    png(file.path("~", "Desktop", "distOfBhats_Xrnd_01.png"), width=1800, height=1000, pointsize=24)
}
par(mfrow=c(1, 2))
hist(mxbhats[ ,1], cex=1.4, lwd=2, xlim=c(-40, 40), main=paste0("Sim b1 with random X -- sim pred var = ", round(xxx2[1], 2)))
abline(v=b[1], col="#00FF00", lwd=4)
hist(mxbhats[ ,2], cex=1.4, lwd=2, xlim=c(-40, 40), main=paste0("Sim b2 with random X -- sim pred var = ", round(xxx2[2], 2)))
abline(v=b[2], col="#00FF00", lwd=4)
if(xbool_save_file) { dev.off() }





