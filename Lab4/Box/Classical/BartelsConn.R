BartelsConn <- function(x) {
  if (missing(x)) {
    y <- list(
      nx = 2,
      ng = 0,
      nh = 0,
      xl = function(nx) get_xl(nx),
      xu = function(nx) get_xu(nx),
      fmin = function(nx) get_fmin(nx),
      xmin = function(nx) get_xmin(nx),
      features = c(0, 0, 0, 0, 1, 0, 1, 0),
      libraries = c(0, 0, 0, 1, 1, 0, 0, 0, 0, 0)
    )
    return(y)
  }
  

  
  y <- abs(x[1]^2 + x[2]^2 + x[1]*x[2]) + abs(sin(x[1])) + abs(cos(x[2]))
  return(y)
}

get_xl <- function(nx) {
  return(rep(-500, nx))
}

get_xu <- function(nx) {
  return(rep(500, nx))
}

get_fmin <- function(nx) {
  return(1)
}

get_xmin <- function(nx) {
  return(rep(0, nx))
}
