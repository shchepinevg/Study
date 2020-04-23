func <- function(x, y, sd1, sd2, m1, m2) {
  const <- 1/(2*pi*sd1*sd2)
  e <- exp(((-1/2)*((x - m1)/sd1)^2) - ((1/2)*((y - m2)/sd2)^2))
  return(const*e)
}

decision <- function(x, y, sd1est, sd2est, m11est, m12est, m21est, m22est, p1est, p2est) {
  f1 <- func(x, y, sd1est, sd2est, m11est, m12est)
  f2 <- func(x, y, sd1est, sd2est, m21est, m22est)
  result <- f1*p1est - f2*p2est
  if (result > 0) {
    return("1")
  } else if (result < 0) {
    return("2")
  } else {
    return("0")
  }
}

classification <- function(n1test, n2test, x1test, x2test, sd1est, sd2est, m11est, m12est,
                           m21est, m22est, p1est, p2est) {
  errorPoints1 <- c()
  errorPoints2 <- c()
  unclearPoints <- c()
  rightPoints1 <- c()
  rightPoints2 <- c()
  
  for (i in 1:n1test) {
    if (decision(x1test[i,1], x1test[i,2], sd1est, sd2est, m11est, m12est,
                       m21est, m22est, p1est, p2est) == "2") {
      errorPoints1 <- c(errorPoints1, x1test[i,])
    } else if (decision(x1test[i,1], x1test[i,2], sd1est, sd2est, m11est, m12est,
                              m21est, m22est, p1est, p2est) == "0") {
      unclearPoints <- c(unclearPoints, x1test[i,])
    } else {
      rightPoints1 <- c(rightPoints1, x1test[i,])
    }
  }
  
  for (i in 1:n2test) {
    if (decision(x2test[i,1], x2test[i,2], sd1est, sd2est, m11est, m12est,
                       m21est, m22est, p1est, p2est) == "1") {
      errorPoints2 <- c(errorPoints2, x2test[i,])
    } else if (decision(x2test[i,1], x2test[i,2], sd1est, sd2est, m11est, m12est,
                              m21est, m22est, p1est, p2est) == "0") {
      unclearPoints <- c(unclearPoints, x2test[i,])
    } else {
      rightPoints2 <- c(rightPoints2, x2test[i,])
    }
  }
  
  if (!is.null(errorPoints1)) {
    errorPoints1 <- matrix(errorPoints1, ncol = 2, byrow = TRUE)
  }
  
  if (!is.null(errorPoints2)) {
    errorPoints2 <- matrix(errorPoints2, ncol = 2, byrow = TRUE)
  }
  
  if (!is.null(unclearPoints)) {
    unclearPoints <- matrix(unclearPoints, ncol = 2, byrow = TRUE)
  }
  
  rightPoints1 <- matrix(rightPoints1, ncol = 2, byrow = TRUE)
  rightPoints2 <- matrix(rightPoints2, ncol = 2, byrow = TRUE)
  errors <- rbind(errorPoints1, errorPoints2)
  
  err <- 0
  if (!is.null(errors)) {
    err <- nrow(errors)/(n1test+n2test) * 100
  }
  
  unclear <- 0
  if (!is.null(unclearPoints)) {
    unclear <- nrow(unclearPoints)
  }
  
  list(rightPoints1 = rightPoints1, rightPoints2 = rightPoints2, errors = errors, 
       errorPoints1 = errorPoints1, errorPoints2 = errorPoints2, err = err, 
       unclearPoints = unclearPoints, unclear <- unclear)
}