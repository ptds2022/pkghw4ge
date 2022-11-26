library(tidyverse)

#' @title Bootstrap function Modification
#' @author Group-E
#' @description This bootstrap function will provide you always an object of class 'bootstrap'
#' @importFrom stats runif
#' @export
#' @examples
#' set.seed(566)
#' class(Bootstrap(rnorm(100), statistic = mean))
bootstrap <- function(x, B = 1000, statistic, sample_size = length(x)){
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

#' @title Bootstrap Print
#' @author Group-E
#' @description This bootstrap function is a modification of the the bootstrap.function with a different output and the estimated of the variance.
#' @importFrom stats runif
#' @export
#' @examples
#' set.seed(566)
#' boot2 <- Bootstrap(matrix(rnorm(200),nc=2), statistic = var)
#' boot2
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

#' @title Summary Bootstrap
#' @author Group-E
#' @description Return an object of class summary.bootstrap that will represent a summary of the bootstrap class.
#' @importFrom stats runif
#' @export
#' @examples
#' summary(boot)
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

#' @title Summary Bootstrap Mod
#' @author Group-E
#' @description Modification of summary.bootstrap function to obtain "summary(boot, level = 1.2)" and "summary(boot, level = .8)"
#' @importFrom stats runif
#' @export
#' @examples
#' summary(boot, level = 1.2)
summary.bootstrap.mod <- function(x, level = 0.95){

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

#' @title Plot Boot H
#' @author GroupE
#' @description Provide a vertical box-plot of the class bootstrap
#' @importFrom stats runif
#' @export
#' @examples
#' plot(boot)
plot.bootstrap.h <- function(x){
  boxplot(x$theta_star, col = "darkgreen", main = "Bootstrap estimates", pch = 19 )
}

#' @title Plot Boot V
#' @author Group-E
#' @description Provide an horizontal box-plot of the class bootstrap
#' @importFrom stats runif
#' @export
#' @examples
#' plot(boot, main = "Test", cex=2, horizontal = T)
plot.bootstrap.v <- function(x, main ="Bootstrap estimates", pch = 19 , cex = 1 , horizontal = F){
  boxplot(x$theta_star, col = "darkgreen", main = main, pch = pch, cex = cex , horizontal = horizontal)
}
