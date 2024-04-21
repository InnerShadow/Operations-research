AckleyN4 <- function(x) {
  if (missing(x)) {
    y <- list(
      nx = 2,
      ng = 0,
      nh = 0,
      xl = function(nx) get_xl(nx),
      xu = function(nx) get_xu(nx),
      fmin = function(nx) get_fmin(nx),
      xmin = function(nx) get_xmin(nx),
      features = c(1, 0, 1, 1, 0, 0, 0, 0),
      libraries = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
    )
    return(y)
  }
  

  
  y <- sum(exp(-0.2) * sqrt(x[1:(length(x) - 1)]^2 + x[2:length(x)]^2) +
             3 * (cos(2 * x[1:(length(x) - 1)]) + sin(2 * x[2:length(x)])))
  return(y)
}

get_xl <- function(nx) {
  return(rep(-35, nx))
}

get_xu <- function(nx) {
  return(rep(35, nx))
}

get_fmin <- function(nx) {
  return(-4.59010163415866756509)
}

get_xmin <- function(nx) {
  return(c(-1.50962010564600035423, -0.75486511728330185633))
}
