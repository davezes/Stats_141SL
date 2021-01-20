


options(stringsAsFactors=FALSE)

xbool_save_file <- FALSE

####################

N <- 1000
d <- 7

set.seed(777)

X <- cbind(rep(1, N), matrix(rnorm(N*d), N, d))
colnames(X) <- c("int", paste0("x", 1:d))

head(X)


b_true <- rep_len( c(-1/3, 1/3), d+1 )

y_true <- X %*% b_true

xsig <- 3

y <- y_true + rnorm(N, 0, xsig)


############ use lm

xlm <- lm(y~X-1)

summary(xlm)





#############

inv_tXX <- solve( crossprod(X) )

b_hat <- inv_tXX %*% t(X) %*% y

b_hat


y_hat <- X %*% b_hat


xsig_hat <- sqrt( mean( (y - y_hat)^2 ) ) ; xsig_hat #### why is this different from summary?


xsig_hat <- sqrt( sum( (y - y_hat)^2 ) / (N-d-1) ) ; xsig_hat #### why is this different from summary?


xSE_hats <- xsig_hat * sqrt( diag( inv_tXX ) )


t(t( #### compare with summary
xSE_hats
))





t_score <- b_hat / xSE_hats ; t_score

p_vals <- 2 * pt( - abs(t_score), (N-d-1) ) ; p_vals









