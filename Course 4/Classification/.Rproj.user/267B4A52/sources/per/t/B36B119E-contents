centralLimTheor <- function(m1, m2, sd1, sd2, k) {
  sum1 <- 0
  sum2 <- 0
  for (i in 1:k) {
    sum1 <- sum1 + (runif(1) - 0.5)
    sum2 <- sum2 + (runif(1) - 0.5)
  }
  
  res1 <- sd1*sqrt(12/k)*sum1 + m1
  res2 <- sd2*sqrt(12/k)*sum2 + m2
  
  return(c(res1, res2))
}

polarCoordinatMeth <- function(m1, m2, sd1, sd2) {
  s = 1
  while(s >= 1) {
    v1 = 2*runif(1) - 1
    v2 = 2*runif(1) - 1
    s = v1^2 + v2^2
  }
  
  x1 = v1*sqrt((-2)*log(s)/s)
  x2 = v2*sqrt((-2)*log(s)/s)
  
  return(c(sd1*x1 + m1, sd2*x2 + m2))
}

generatePoints <- function(typeGener, m1, m2, sd1, sd2, k) {
  if (typeGener == "Central limit theorem") {
    return(centralLimTheor(m1, m2, sd1, sd2, k))
  } else if (typeGener == "Polar coords") {
    return(polarCoordinatMeth(m1, m2, sd1, sd2))
  }
}

sampleGeneration <- function(typeGener, ntrain, ntest, m11, m12, sd11, sd12, m21, m22, sd21, sd22, k, p1) {
  vec1 <- c()
  vec2 <- c()
  for (i in 1:ntrain) {
    if (runif(1) < p1) {
      vec1 <- c(vec1, generatePoints(typeGener, m11, m12, sd11, sd12, k))
    } else {
      vec2 <- c(vec2, generatePoints(typeGener, m21, m22, sd21, sd22, k))
    }
  }
  x1train <- matrix(vec1, ncol = 2, byrow = TRUE)
  x2train <- matrix(vec2, ncol = 2, byrow = TRUE)
  n1train = nrow(x1train)
  n2train = nrow(x2train)
  
  vec1 <- c()
  vec2 <- c()
  for (i in 1:ntest) {
    if (runif(1) < p1) {
      vec1 <- c(vec1, generatePoints(typeGener, m11, m12, sd11, sd12, k))
    } else {
      vec2 <- c(vec2, generatePoints(typeGener, m21, m22, sd21, sd22, k))
    }
  }
  x1test <- matrix(vec1, ncol = 2, byrow = TRUE)
  x2test <- matrix(vec2, ncol = 2, byrow = TRUE)
  n1test = nrow(x1test)
  n2test = nrow(x2test)
  
  list(x1train = x1train, x2train = x2train, n1train = n1train, n2train = n2train,
       x1test = x1test, x2test = x2test, n1test = n1test, n2test = n2test)
}

#################################################











