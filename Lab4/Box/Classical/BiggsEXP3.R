BiggsEXP3 <- function(x) {
  if (missing(x)) {
    y <- list(
      nx = 3,
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
  

  
  t <- seq(0.1, 0.9, by = 0.1)
  y <- sum((exp(-t*x[1]) - x[3]*exp(-t*x[2]) - exp(-t) + 5*exp(-t*10))^2)
  return(y)
}

get_xl <- function(nx) {
  return(rep(0, nx))
}

get_xu <- function(nx) {
  return(rep(20, nx))
}

get_fmin <- function(nx) {
  return(0)
}

get_xmin <- function(nx) {
  return(c(1, 10, 5))
}
