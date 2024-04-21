Brent <- function(x) {
  if (missing(x)) {
    return(list(
      nx = 2,
      ng = 0,
      nh = 0,
      xl = get_xl,
      xu = get_xu,
      fmin = get_fmin,
      xmin = get_xmin,
      features = c(1, 0, 0, 0, 1, 0, 0, 1),
      libraries = c(0, 0, 0, 1, 1, 0, 0, 0, 0, 0)
    ))
  }
  
  if (length(dim(x)) == 2 && ncol(x) > nrow(x)) {
    x <- t(x)
  }
  
  y <- (x[1] + 10)^2 + (x[2] + 10)^2 + exp(-x[1]^2 - x[2]^2)
  
  return(y)
}

get_xl <- function(nx) {
  return(rep(-10, nx))
}

get_xu <- function(nx) {
  return(rep(10, nx))
}

get_fmin <- function(nx) {
  return(0)
}

get_xmin <- function(nx) {
  return(c(-10, -10))
}
