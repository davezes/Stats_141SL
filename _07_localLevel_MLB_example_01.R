



xdf_mlbSub <- read.table("mlb_filter_example.tsv", header=TRUE, sep="\t", quote="\"")

tail(xdf_mlbSub)
dim(xdf_mlbSub)

############## the field PID_ndx gives the row index for the last
############## appearance of pitcher
############## gives zero if first appearance in data


############
x <- as.vector(xdf_mlbSub[ , "x" ])


###########
xndx <- xdf_mlbSub[ , "PID_ndx" ]

N <- nrow(xdf_mlbSub)







###################### g is our filter's 'gain'
g     <- rep(1, N)

###################### this vector will hold our a-posteriori predictions
xhat <- numeric(N)

###################### this vector will hold our a-prior predictions
xhat_forecast <- numeric(N)

################### signal to noise ratio.  Sometimes called rho.
################### controls amount of 'forgetting'
################### when zero, filter 'remembers' everything
################### the larger the value, the more the filter 'forgets' the past
################### this is really the only tuning parameter in this filter
SNR <- 1/1000

for(ii in 1:N) {
    
    xthis_ndx <- xndx[ ii ]
    
    ######################## if first appearance of pitcher, set to initial values
    if(xthis_ndx == 0) {
        xprevious_xhat <- 0
        xprevious_g <- 1
    } else { ############## otherwise get previous values
        xprevious_xhat <- xhat[ xthis_ndx ]
        xprevious_g <- g[ xthis_ndx ]
    }
    
    xhat_forecast[ ii ] <- xprevious_xhat
    
    xthis <- x[ ii ]
    
    xhat[ ii ] <- xprevious_xhat + xprevious_g * (xthis  - xprevious_xhat)
    
    g[ ii ] <- (xprevious_g + SNR) / (xprevious_g + SNR + 1)

}

sqrt( mean( ( x - xhat_forecast )^2 ) )



############# question:  what value of SNR gives the lowest forecast RMSE ?

