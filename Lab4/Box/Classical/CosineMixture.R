CosineMixture <- function(x) {
  if (missing(x)) {
    return(list(nx = 0,
                ng = 0,
                nh = 0,
                xl = function(nx) get_xl(nx),
                xu = function(nx) get_xu(nx),
                fmin = function(nx) get_fmin(nx),
                xmin = function(nx) get_xmin(nx),
                features = c(0, 1, 1, 1, 0, 0, 1, 1),
                libraries = c(0, 0, 0, 1, 1, 0, 0, 0, 0, 0)))
  }
  

  
  y <- -(0.1 * sum(cos(x * 5 * pi)) - sum(x^2))
  
  return(y)
}

get_xl <- function(nx) {
  return(rep(-1, nx))
}

get_xu <- function(nx) {
  return(rep(1, nx))
}

get_fmin <- function(nx) {
  return(-0.1 * nx)
}

get_xmin <- function(nx) {
  return(rep(0, nx))
}
