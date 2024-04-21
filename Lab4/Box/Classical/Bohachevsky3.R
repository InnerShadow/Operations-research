Bohachevsky3 <- function(x) {
  if (missing(x)) {
    y <- list(
      nx = 2,
      ng = 0,
      nh = 0,
      xl = function(nx) get_xl(nx),
      xu = function(nx) get_xu(nx),
      fmin = function(nx) get_fmin(nx),
      xmin = function(nx) get_xmin(nx),
      features = c(1, 0, 0, 1, 0, 0, 1, 0),
      libraries = c(1, 1, 0, 1, 1, 0, 0, 0, 0, 0)
    )
    return(y)
  }
  

  
  y = x[1]^2 + 2*x[2]^2 - 0.3*cos(3*pi*x[1] + 4*pi*x[2]) + 0.3
  return(y)
}

get_xl <- function(nx) {
  return(rep(-100, nx))
}

get_xu <- function(nx) {
  return(rep(100, nx))
}

get_fmin <- function(nx) {
  return(0)
}

get_xmin <- function(nx) {
  return(rep(0, nx))
}
