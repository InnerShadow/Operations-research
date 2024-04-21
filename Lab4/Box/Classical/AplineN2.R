AlpineN2 <- function(x) {
  if (missing(x)) {
    y <- list(
      nx = 0,
      ng = 0,
      nh = 0,
      xl = function(nx) get_xl(nx),
      xu = function(nx) get_xu(nx),
      fmin = function(nx) get_fmin(nx),
      xmin = function(nx) get_xmin(nx),
      features = c(1, 1, 1, 1, 0, 0, 0, 1),
      libraries = c(0, 0, 0, 1, 1, 0, 0, 0, 0, 0)
    )
    return(y)
  }
  

  
  y <- -prod(sqrt(abs(x)) * sin(abs(x)))
  return(y)
}

get_xl <- function(nx) {
  return(rep(0, nx))
}

get_xu <- function(nx) {
  return(rep(10, nx))
}

get_fmin <- function(nx) {
  return(-2.80813118000700523424^nx)
}

get_xmin <- function(nx) {
  return(rep(7.91705268212913271242, nx))
}
