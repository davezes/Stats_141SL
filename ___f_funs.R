



f_dummy <- function(x) {
    u.x <- sort( unique(x) )
    d <- length(u.x)
    mx.out <- matrix(0, length(x), d)
    for(i in 1:d) {
        mx.out[ , i] <- as.integer( u.x[i] == x )
    }
    colnames(mx.out) <- u.x
    return(mx.out)
}






