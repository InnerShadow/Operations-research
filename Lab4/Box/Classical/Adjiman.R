Adjiman <- function(x) {
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
      libraries = c(0, 0, 0, 1, 1, 0, 0, 0, 0, 0)
    )
    return(y)
  }
  

  
  y <- cos(x[1]) * sin(x[2]) - (x[1] / (x[2]^2 + 1))
  return(y)
}

get_xl <- function(nx) {
  return(rep(-1, nx))
}

get_xu <- function(nx) {
  return(rep(2, nx))
}

get_fmin <- function(nx) {
  return(-2.02180678335938734946)
}

get_xmin <- function(nx) {
  return(c(1.99999999999963140596, 0.10578346946858041555))
}
