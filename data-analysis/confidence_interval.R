# Confidence interval calculator

#Confidence intervals | used t-distribution
cicalc <- function(data){
  x_bar <- mean(data)
  sigma <- sd(data)
  n.sim <- length(data)
  #error <- qnorm(0.975)*s/sqrt(n)
  A = qt(0.975, df=n.sim-1)*sigma/sqrt(n.sim)
  left <- x_bar - A
  right <- x_bar + A
  ci_interval <- c(round(left,digits=2),round(right,digits=2))
  return(ci_interval)
}

lower_cicalc <- function(data){
  x_bar <- mean(data)
  sigma <- sd(data)
  n.sim <- length(data)
  #error <- qnorm(0.975)*s/sqrt(n)
  A = qt(0.975, df=n.sim-1)*sigma/sqrt(n.sim)
  left <- x_bar - A
  return(round(left,digits=2))
}

upper_cicalc <- function(data){
  x_bar <- mean(data)
  sigma <- sd(data)
  n.sim <- length(data)
  #error <- qnorm(0.975)*s/sqrt(n)
  A = qt(0.975, df=n.sim-1)*sigma/sqrt(n.sim)
  right <- x_bar + A
  return(round(right,digits=2))
}