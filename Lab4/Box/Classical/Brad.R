Brad <- function(x) {
  if (missing(x)) {
    return(list(
      nx = 3,
      ng = 0,
      nh = 0,
      xl = get_xl,
      xu = get_xu,
      fmin = get_fmin,
      xmin = get_xmin,
      features = c(1, 0, 0, 1, 0, 0, 0, 0),
      libraries = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
    ))
  }
  
  if (length(dim(x)) == 2 && ncol(x) > nrow(x)) {
    x <- t(x)
  }
  
  T <- c(0.14, 0.18, 0.22, 0.25, 0.29, 0.32, 0.35, 0.39,
         0.37, 0.58, 0.73, 0.96, 1.34, 2.10, 4.39)
  
  y <- 0
  for (i in 1:15) {
    a <- 16 - i
    b <- min(i, a)
    y <- y + ((T[i] - x[1] - i) / (a * x[2] + b * x[3]))^2
  }
  
  return(y)
}

get_xl <- function(nx) {
  return(c(-0.25, 0.01, 0.01))
}

get_xu <- function(nx) {
  return(c(0.25, 2.5, 2.5))
}

get_fmin <- function(nx) {
  return(6.93522806970521621395)
}

get_xmin <- function(nx) {
  return(c(-0.25, 2.5, 2.5))
}
