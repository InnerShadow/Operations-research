Bird <- function(x) {
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
      libraries = c(0, 0, 0, 1, 1, 0, 1, 0, 0, 0)
    )
    return(y)
  }
  

  
  y = (x[1] - x[2])^2 + exp((1 - sin(x[1]))^2)*cos(x[2]) + 
    exp((1 - cos(x[2]))^2)*sin(x[1])
  return(y)
}

get_xl <- function(nx) {
  return(rep(-2*pi, nx))
}

get_xu <- function(nx) {
  return(rep(2*pi, nx))
}

get_fmin <- function(nx) {
  return(-106.76453674926477788176)
}

get_xmin <- function(nx) {
  return(c(4.70104312998214268760, 3.15293850371239692265))
}
