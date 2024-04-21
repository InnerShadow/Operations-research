AckleyN3 <- function(x) {
  if (missing(x)) {
    y <- list(
      nx = 2,
      ng = 0,
      nh = 0,
      xl = function(nx) get_xl(nx),
      xu = function(nx) get_xu(nx),
      fmin = function(nx) get_fmin(nx),
      xmin = function(nx) get_xmin(nx),
      features = c(1, 0, 0, 1, 0, 0, 0, 0),
      libraries = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
    )
    return(y)
  }
  

  
  y <- -200 * exp(-0.02 * sqrt(x[1]^2 + x[2]^2)) +
    5 * exp(cos(3 * x[1]) + sin(3 * x[2]))
  return(y)
}

get_xl <- function(nx) {
  return(rep(-32, nx))
}

get_xu <- function(nx) {
  return(rep(32, nx))
}

get_fmin <- function(nx) {
  return(-195.62902826227937680414)
}

get_xmin <- function(nx) {
  return(c(0.68257717405194995308, -0.36070185844338453762))
}
