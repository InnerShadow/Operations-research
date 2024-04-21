Bukin4 <- function(x) {
  if (missing(x)) {
    return(list(
      nx = 2,
      ng = 0,
      nh = 0,
      xl = get_xl,
      xu = get_xu,
      fmin = get_fmin,
      xmin = get_xmin,
      features = c(0, 0, 0, 0, 0, 0, 1, 0),
      libraries = c(0, 0, 0, 1, 1, 0, 1, 0, 0, 0)
    ))
  }
  
  if (length(dim(x)) == 2 && ncol(x) > nrow(x)) {
    x <- t(x)
  }
  
  y <- 100 * x[2]^2 + 0.01 * abs(x[1] + 10)
  
  return(y)
}

get_xl <- function(nx) {
  return(c(-15, -3))
}

get_xu <- function(nx) {
  return(c(-5, 3))
}

get_fmin <- function(nx) {
  return(0)
}

get_xmin <- function(nx) {
  return(c(-10, 0))
}
