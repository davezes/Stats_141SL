

options(stringsAsFactors=FALSE, width=350)


####### make sure in working directory
##source("___f_funs.R")

xbool_save_file <- FALSE

############# sim data

set.seed(777)

xsigma <- 1

n <- 20


xrndAssignDom <- c("Control", "Tx")

H0delta <- 0
HtrueDelta <- 1



nn <- 500000



xtvals_null <- rep(NA, nn)
xtvals_true <- rep(NA, nn)

for(i in 1:nn) {
    
    x <- sample(xrndAssignDom, size=n, replace=TRUE)
    
    maskC <- x %in% c("Control")
    maskTx <- x %in% c("Tx")
    
    nC <- sum(maskC)
    nTx <- sum(maskTx)
    
    y_null <- numeric(n)
    y_null[ maskC ] <- rnorm(nC, 0, xsigma)
    y_null[ maskTx ] <- rnorm(nTx, H0delta, xsigma)

    s2pooled_null <- ((nC-1)*var(y_null[ maskC ]) + (nTx-1)*var(y_null[ maskTx ])) / (nC + nTx - 2)
    tval_null <- (mean(y_null[ maskTx ]) - mean(y_null[ maskC ]) - H0delta) / sqrt( s2pooled_null * (1/nC + 1/nTx) ) ### correct
    xtvals_null[i] <- tval_null
    
    
    
    y_true <- numeric(n)
    y_true[ maskC ] <- rnorm(sum(maskC), 0, xsigma)
    y_true[ maskTx ] <- rnorm(sum(maskTx), HtrueDelta, xsigma)
    
    s2pooled_true <- ((nC-1)*var(y_true[ maskC ]) + (nTx-1)*var(y_true[ maskTx ])) / (nC + nTx - 2)
    tval_true <- (mean(y_true[ maskTx ]) - mean(y_true[ maskC ]) - H0delta) / sqrt( s2pooled_true * (1/nC + 1/nTx) ) ### correct
    xtvals_true[i] <- tval_true
    
    
}


tcrit <- quantile(xtvals_null, 0.99, na.rm=TRUE) ; tcrit

if(xbool_save_file) {
    png(file.path("~", "Desktop", "t_power_trueRandomAssignment_01.png"), width=1000, height=1000, pointsize=24)
}
par(mfrow=c(2,1), mar=c(2.5,3,2,1))
xlims <- range( c( xtvals_null, xtvals_true) )
xlims <- c(-6, 8)
hist(xtvals_null, xlim=xlims)
abline(v=tcrit, col="#FF3333", lwd=3)
hist(xtvals_true, xlim=xlims)
abline(v=tcrit, col="#FF3333", lwd=3)
if(xbool_save_file) { dev.off() }

cat("Sim True-Random Assignment Power: ", sum(xtvals_true > tcrit, na.rm=TRUE) / nn, "\n")






########## non-fully random assignment -- n / 2

xtvals_null <- rep(NA, nn)
xtvals_true <- rep(NA, nn)

for(i in 1:nn) {
    
    x <- rep_len( xrndAssignDom, n )
    
    maskC <- x %in% c("Control")
    maskTx <- x %in% c("Tx")
    
    nC <- sum(maskC)
    nTx <- sum(maskTx)
    
    y_null <- numeric(n)
    y_null[ maskC ] <- rnorm(nC, 0, xsigma)
    y_null[ maskTx ] <- rnorm(nTx, H0delta, xsigma)

    s2pooled_null <- ((nC-1)*var(y_null[ maskC ]) + (nTx-1)*var(y_null[ maskTx ])) / (nC + nTx - 2)
    tval_null <- (mean(y_null[ maskTx ]) - mean(y_null[ maskC ]) - H0delta) / sqrt( s2pooled_null * (1/nC + 1/nTx) ) ### correct
    xtvals_null[i] <- tval_null
    
    
    
    y_true <- numeric(n)
    y_true[ maskC ] <- rnorm(sum(maskC), 0, xsigma)
    y_true[ maskTx ] <- rnorm(sum(maskTx), HtrueDelta, xsigma)
    
    s2pooled_true <- ((nC-1)*var(y_true[ maskC ]) + (nTx-1)*var(y_true[ maskTx ])) / (nC + nTx - 2)
    tval_true <- (mean(y_true[ maskTx ]) - mean(y_true[ maskC ]) - H0delta) / sqrt( s2pooled_true * (1/nC + 1/nTx) ) ### correct
    xtvals_true[i] <- tval_true
    
    
}

tcrit <- quantile(xtvals_null, 0.99, na.rm=TRUE) ; tcrit

if(xbool_save_file) {
    png(file.path("~", "Desktop", "t_power_semiRandomAssignment_01.png"), width=1000, height=1000, pointsize=24)
}
par(mfrow=c(2,1), mar=c(2.5,3,2,1))
xlims <- range( c( xtvals_null, xtvals_true) )
xlims <- c(-6, 8)
hist(xtvals_null, xlim=xlims)
abline(v=tcrit, col="#FF3333", lwd=3)
hist(xtvals_true, xlim=xlims)
abline(v=tcrit, col="#FF3333", lwd=3)
if(xbool_save_file) { dev.off() }


cat("Sim 'Semi-Random' Assignment Power: ", sum(xtvals_true > tcrit, na.rm=TRUE) / nn, "\n")






