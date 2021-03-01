

################# let's simulate a data set
################# 100 students, 1 writen response each
################# For each writen response, 5 raters

######### random simulation

set.seed(777)

N <- 100

xrater <- rep( c("AI", "TA1", "TA2", "Prof1", "Prof2"), N )

xstudent <- as.character( rep( 1:N, each=5 ) )

xscore <- sample( c(1, 2, 3), 5*N, replace=TRUE )

xdf <- data.frame("score"=xscore, "student"=xstudent, "rater"=xrater)

head(xdf, 14)


############# the aov() function!
############# let's assume no student x rater interaction
xaov <- aov(score ~ student + rater, data=xdf)

summary(xaov)

sum( summary(xaov)[[1]][ , "Sum Sq" ] )

sum( (xscore - mean(xscore))^2 ) #### total SS



############# model with student x rater interaction
xaov <- aov(score ~ student * rater, data=xdf)

summary(xaov)

sum( summary(xaov)[[1]][ , "Sum Sq" ] )

sum( (xscore - mean(xscore))^2 ) #### total SS

########### NOTICE the model consumes all 500 dfs


############


