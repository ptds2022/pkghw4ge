library(tidyverse)


# Using your most efficient implementation of the bootstrap from the previous homework,
# modify the output to return an object of class bootstrap.

Bootstrap <- function(x, B = 1000, statistic, sample_size = length(x)){
  coerced_to_a_vector <- FALSE
  if(any(!is.numeric(x))){
    stop("'x' must be numeric")
  }
  if(any(!is.numeric(B))){
    stop("'B' must be numeric")
  }
  if(any(!is.function(statistic))){
    stop("'statistic' must be a function")
  }
  if(any(!is.vector(x))){
    x <- as.vector(x)
    coerced_to_a_vector <- TRUE
    warning("'x' has been coerced to a vector")
  }
  if(any(is.na(x))){
    x <- x[!is.na(x)]
    warning("some NA in 'x' has been removed")
    if(is.null(match.call()$sample_size)){
      sample_size <- length(x)
      warning("readjustment of the default sample size, new size =", sample_size)
    }
  }
  theta_hat <- statistic(x)
  Bootsptrapped_list <- 1:B
  for (i in 1:B){Bootsptrapped_list[i] <-  statistic(rnorm(n= sample_size, mean= mean(x), sd=sqrt(var(x))))}
  Unbiased_estimator <- 0
  for (i in 1:B){Unbiased_estimator <- Unbiased_estimator + (Bootsptrapped_list[i]- theta_hat)**2}
  Unbiased_estimator <- Unbiased_estimator/(B -1 )
  theta_star <- Bootsptrapped_list
  varBoot <- var(Bootsptrapped_list)
  Unbiased_estimator <- Unbiased_estimator
  final_list <- structure(list("theta_hat"= theta_hat,
                               "theta_star"= theta_star,
                               "varBoot"=varBoot,
                               "Unbiased_estimator"=Unbiased_estimator,
                               "coerced_to_a_vector" = coerced_to_a_vector,
                               "stat"= as.character(substitute(statistic)),
                               "B" = B,
                               "sample_size" = sample_size), class= "bootstrap")
  return(final_list)
}

set.seed(566)
class(Bootstrap(rnorm(100), statistic = mean))

# question 2


print.bootstrap <- function(x){
  if (x$coerced_to_a_vector == FALSE){
    cat(paste("Non-parametric bootstrap estimate of the variance of ", "'",x$stat,"'\n", sep = ""))
  }
  else {cat(paste("Non-parametric bootstrap for ", "'",x$stat,"'\n", sep = ""))}
  cat("-----------------\n")
  cat(paste("Variance estimate is", round(x$varBoot,4), "\n"))
  cat(paste("Based on B=", x$B, " bootstrap replicates\n", sep = ""))
  cat(paste("and a sample of size:",x$sample_size, "\n"))
}

set.seed(700)
boot <- Bootstrap(rnorm(100), statistic = var)
boot

set.seed(566)
boot2 <- Bootstrap(matrix(rnorm(200),nc=2), statistic = var)
boot2


### question 3

summary.bootstrap <- function(x, level = 0.95){
  cat(paste("Non-parametric bootstrap for ", "'",x$stat,"'\n", sep = ""))
  cat(paste("Number of bootstrap replicates:",x$B, "\n"))
  cat("\n")
  cat(paste("Statistic on the sample 'x': ", x$stat,"(x)=",round(x$theta_hat, 2),"'\n", sep = ""))

  if (x$coerced_to_a_vector == TRUE){
    cat("Notice: 'x' was coerced to a vector\n")
  }

  Z <- qnorm(p= (1-level)/2, lower.tail = FALSE)
  CI <- c(x$theta_hat - sqrt(x$varBoot)*Z, x$theta_hat + sqrt(x$varBoot)*Z)
  cat("\n")
  cat(paste("Confidence interval at the ", level*100,"%-level:\n", sep = ""))
  cat(round(CI, 2),"\n")
  cat("\n")
  cat("Bootstrap estimates:\n")
  summary(x$theta_star, digits=2)
}

summary(boot)

summary(boot2)

### ex 4

summary(boot, level = .8)
# this one ok


# summary(boot, level = 1.2)
# need changes in the summary function

summary.bootstrap <- function(x, level = 0.95){

  if(any(level > 1) | (level< 0)){
    stop("'level' must be in (0,1)")
  }

  cat(paste("Non-parametric bootstrap for ", "'",x$stat,"'\n", sep = ""))
  cat(paste("Number of bootstrap replicates:",x$B, "\n"))
  cat("\n")
  cat(paste("Statistic on the sample 'x': ", x$stat,"(x)=",round(x$theta_hat, 2),"'\n", sep = ""))

  if (x$coerced_to_a_vector == TRUE){
    cat("Notice: 'x' was coerced to a vector\n")
  }

  Z <- qnorm(p= (1-level)/2, lower.tail = FALSE)
  CI <- c(x$theta_hat - sqrt(x$varBoot)*Z, x$theta_hat + sqrt(x$varBoot)*Z)
  cat("\n")
  cat(paste("Confidence interval at the ", level*100,"%-level:\n", sep = ""))
  cat(round(CI, 2),"\n")
  cat("\n")
  cat("Bootstrap estimates:\n")
  summary(x$theta_star, digits=2)
}

summary(boot, level = 1.2)
# now we get the error



#print(summary(boot), digits=5)
## need change on the print and the summary function

summary.bootstrap <- function(x, level = 0.95, digits= 2, ...){

  if(any(level > 1) | (level< 0)){
    stop("'level' must be in (0,1)")
  }

  Z <- qnorm(p= (1-level)/2, lower.tail = FALSE)
  CI1 <- c(x$theta_hat - sqrt(x$varBoot)*Z, x$theta_hat + sqrt(x$varBoot)*Z)
  CI <- round(CI1, digits = digits)
  sto1 <- x$theta_hat
  sto <- round(sto1, digits= digits )
  cat(paste("Non-parametric bootstrap for ", "'",x$stat,"'\n", sep = ""))
  cat(paste("Number of bootstrap replicates:",x$B, "\n"))
  cat("\n")
  cat(paste("Statistic on the sample 'x': ", x$stat,"(x)=",sto,"\n", sep = ""))

  if (x$coerced_to_a_vector == TRUE){
    cat("Notice: 'x' was coerced to a vector\n")
  }

  cat("\n")
  cat(paste("Confidence interval at the ", level*100,"%-level:\n", sep = ""))
  cat(CI,"\n")
  cat("\n")
  cat("Bootstrap estimates:\n")
  print(summary(x$theta_star),  digits= digits)

  return(invisible(structure(list("summary"= summary(x$theta_star),
                                  "CI" = CI1,
                                  "sto"= sto1,
                                  "coerced_to_a_vector"=x$coerced_to_a_vector,
                                  "level"= level,
                                  "stat" = x$stat,
                                  "B" = x$B), class = "summary.bootstrap")))
}




print.summary.bootstrap <- function(x, digits = 2){
  CI <- x$CI
  CI <- round(CI, digits = digits)
  sto <- x$sto
  sto <- round(sto, digits= digits )
  cat(paste("\n\n\n\ ----------------- \n\nWith application of", digits,  "decimal places\n\n"))
  cat(paste("Non-parametric bootstrap for ", "'",x$stat,"'\n", sep = ""))
  cat(paste("Number of bootstrap replicates:",x$B, "\n"))
  cat("\n")
  cat(paste("Statistic on the sample 'x': ", x$stat,"(x)=",sto,"\n", sep = ""))

  if (x$coerced_to_a_vector == TRUE){
    cat("Notice: 'x' was coerced to a vector\n")
  }

  cat("\n")
  cat(paste("Confidence interval at the ", x$level*100,"%-level:\n", sep = ""))
  cat(CI,"\n")
  cat("\n")
  cat("Bootstrap estimates:\n")
  print(x$summary, digits = digits)
  cat("\n")
}

print(summary(boot), digits=5) ### why doing this ????

## instead we can do this and it's work
summary(boot, digits =5)

#######
set.seed(300)
summary(Bootstrap(matrix(rnorm(200),nc=2), statistic = median, B=1e4))

## this one is ok


##### ex.5

#plot(boot)
# we have to do some changes in plot()

plot.bootstrap <- function(x){
  boxplot(x$theta_star, col = "darkgreen", main = "Bootstrap estimates", pch = 19 )
}

plot(boot)

# plot(boot, main = "Test", cex=2, horizontal = T)
# we have to do some changes in plot()


plot.bootstrap <- function(x, main ="Bootstrap estimates", pch = 19 , cex = 1 , horizontal = F){
  boxplot(x$theta_star, col = "darkgreen", main = main, pch = pch, cex = cex , horizontal = horizontal)
}

plot(boot, main = "Test", cex=2, horizontal = T)
