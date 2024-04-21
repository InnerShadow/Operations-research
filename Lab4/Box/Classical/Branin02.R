Branin02 <- function(x) {
  if (missing(x)) {
    return(list(
      nx = 2,
      ng = 0,
      nh = 0,
      xl = get_xl,
      xu = get_xu,
      fmin = get_fmin,
      xmin = get_xmin,
      features = c(1, 0, 0, 1, 0, 0, 0, 0),
      libraries = c(0, 0, 0, 1, 1, 0, 0, 0, 0, 0)
    ))
  }
  
  if (length(dim(x)) == 2 && ncol(x) > nrow(x)) {
    x <- t(x)
  }
  
  a <- 1
  b <- 5.1 / (4 * pi^2)
  c <- 5 / pi
  d <- 6
  e <- 10
  g <- 1 / (8 * pi)
  
  f1 <- a * (x[2] - b * x[1]^2 + c * x[1] - d)^2
  f2 <- e * (1 - g) * cos(x[1]) * cos(x[2])
  f3 <- log(x[1]^2 + x[2]^2 + 1)
  
  y <- (f1 + f2 + f3 + e)
  
  return(y)
}

get_xl <- function(nx) {
  return(c(-5, 0))
}

get_xu <- function(nx) {
  return(c(10, 15))
}

get_fmin <- function(nx) {
  return(5.55891440389381585874)
}

get_xmin <- function(nx) {
  return(c(-3.19698842472849120711, 12.52625788524676586633))
}
