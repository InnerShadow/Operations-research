Beale <- function(x) {
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
      libraries = c(1, 1, 0, 1, 1, 0, 1, 0, 0, 0)
    )
    return(y)
  }
  

  
  y <- (1.5 - x[1]*(1 - x[2]))^2 + (2.25 - x[1]*(1 - x[2]^2))^2 + (2.625 - x[1]*(1 - x[2]^3))^2
  return(y)
}

get_xl <- function(nx) {
  return(rep(-4.5, nx))
}

get_xu <- function(nx) {
  return(rep(4.5, nx))
}

get_fmin <- function(nx) {
  return(0)
}

get_xmin <- function(nx) {
  return(c(3, 0.5))
}
