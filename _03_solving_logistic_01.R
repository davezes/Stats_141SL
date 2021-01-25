


options(stringsAsFactors=FALSE)

xbool_save_file <- FALSE

source( "___f_funs.R" )

#################### simulate

N <- 1000
d <- 7

set.seed(777)

X <- cbind(rep(1, N), matrix(rnorm(N*d), N, d))
colnames(X) <- c("int", paste0("x", 1:d))

head(X)


b_true <- rep_len( c(-1/5, 1/5), d+1 )

y_star <- X %*% b_true

p_true <- 1 / ( 1 + exp(-y_star) )

y <- rbinom(N, 1, p_true)


############ use glm

xglm <- glm(y~X-1, family=binomial(link="logit"))

summary(xglm)







################# solve recursively


b_hat <- rep(0, ncol(X))


for( i in 1:30) {
    phat <- as.vector(  1 / (1 + exp( - X %*% b_hat ))  )
    W <- diag(phat*(1-phat))
    b_hat <- b_hat + solve( t(X) %*% W %*% X ) %*% t(X) %*% (y-phat)
    print(b_hat)
}


xSE_hats <- sqrt( diag( solve( t(X) %*% W %*% X ) ) ) ; xSE_hats

t(t( #### compare with summary
xSE_hats
))


z_score <- b_hat / xSE_hats ; z_score

p_vals <- 2 * pnorm( - abs(z_score) ) ; p_vals

summary(xglm)




############### "Residual deviance" vs logistic cost

y_star_hat <- X %*% b_hat

y_hat <- 1 / ( 1 + exp(-y_star_hat) )

xlogit_cost <- -2 * sum( ( y * log(y_hat) + (1-y) * log(1 - y_hat) ) ) ; xlogit_cost

xlogit_errs <- f_logit_cost(y=y, yhat=y_hat)
sum(xlogit_errs)

######## notice the "Null deviance" is on 1000 df

y_null <- rep(0.5, N)

xlogit_nullerrs <- f_logit_cost(y=y, yhat=y_null)
sum(xlogit_nullerrs)

