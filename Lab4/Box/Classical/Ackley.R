Ackley <- function(x) {
  if (missing(x)) {
    y <- list(
      nx = 0,
      ng = 0,
      nh = 0,
      xl = function(nx) get_xl(nx),
      xu = function(nx) get_xu(nx),
      fmin = function(nx) get_fmin(nx),
      xmin = function(nx) get_xmin(nx),
      features = c(1, 0, 1, 1, 0, 0, 1, 1),
      libraries = c(1, 1, 0, 1, 1, 0, 1, 0, 0, 0)
    )
    return(y)
  }
  

  
  y <- -20 * exp(-0.2 * sqrt((1 / length(x)) * sum(x^2))) -
    exp((1 / length(x)) * sum(cos(2 * pi * x))) + 20 + exp(1)
  return(y)
}

get_xl <- function(nx) {
  return(rep(-15, nx))
}

get_xu <- function(nx) {
  return(rep(30, nx))
}

get_fmin <- function(nx) {
  return(0)
}

get_xmin <- function(nx) {
  return(rep(0, nx))
}