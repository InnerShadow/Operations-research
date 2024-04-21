BoxBetts <- function(x) {
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
      libraries = c(0, 0, 0, 1, 1, 0, 0, 0, 0, 0)
    ))
  }
  
  y <- 0
  for (i in 1:10) {
    y <- y + (exp(-0.1*(i + 1))*x[1] - exp(-0.1*(i + 1))*x[2] - 
                (exp(-0.1*(i + 1)) - exp(-0.1*(i + 1))*x[2]))^2
  }
  
  return(y)
}

get_xl <- function(nx) {
  return(c(0.9, 9, 0.9))
}

get_xu <- function(nx) {
  return(c(1.2, 11.2, 1.2))
}

get_fmin <- function(nx) {
  return(0)
}

get_xmin <- function(nx) {
  return(c(1, 10, 1))
}
