Branin01 <- function(x) {
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
      libraries = c(1, 1, 0, 1, 1, 0, 0, 0, 0, 0)
    ))
  }
  
  if (length(dim(x)) == 2 && ncol(x) > nrow(x)) {
    x <- t(x)
  }
  
  y <- (x[2] - (5.1 / (4 * pi^2)) * x[1]^2 + 5 * x[1] / pi - 6)^2 + 
    10 * (1 - 1 / (8 * pi)) * cos(x[1]) + 10
  
  return(y)
}

get_xl <- function(nx) {
  return(c(-5, 0))
}

get_xu <- function(nx) {
  return(c(10, 15))
}

get_fmin <- function(nx) {
  return(0.39788735772973815585)
}

get_xmin <- function(nx) {
  return(c(3.14159265293527933949, 2.27500000412741654188))
}
