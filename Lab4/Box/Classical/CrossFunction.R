CrossFunction <- function(x) {
  if (missing(x)) {
    return(list(nx = 2,
                ng = 0,
                nh = 0,
                xl = function(nx) get_xl(nx),
                xu = function(nx) get_xu(nx),
                fmin = function(nx) get_fmin(nx),
                xmin = function(nx) get_xmin(nx),
                features = c(0, 0, 0, 1, 0, 0, 0, 1),
                libraries = c(0, 0, 0, 1, 0, 0, 1, 0, 0, 0)))
  }
  

  
  y <- (abs(sin(x[1]) * sin(x[2]) * exp(abs(100 - sqrt(x[1]^2 + x[2]^2)/pi))) + 1)^(-0.1)
  
  return(y)
}

get_xl <- function(nx) {
  return(rep(-10, nx))
}

get_xu <- function(nx) {
  return(rep(10, nx))
}

get_fmin <- function(nx) {
  return(0.00004848221878996157)
}

get_xmin <- function(nx) {
  return(c(-1.34940664297566748075, -1.34940663915170588893))
}
