ABS_5 <- function(x = NULL) {
  if (is.null(x)) {
    return(list(
      nx = 0,
      ng = 0,
      nh = 0,
      xl = function(nx) rep(-100, nx),
      xu = function(nx) rep(100, nx),
      fmin = function(nx) 0,ы
      xmin = function(nx) rep(0, nx),
      features = c(0, 1, 1, 1, 0, 0, 1, 1),
      libraries = c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0)
    ))
  }
  

  
  m <- 0.9000
  k <- 16
  lmbd <- 0.0100
  
  zigzag <- function(xs, k, m, lmbd) {
    xs <- abs(xs)
    xs <- xs / k - floor(xs / k)
    ids <- xs <= lmbd
    valz <- 1 - m + ids * m * (xs / lmbd) + (1 - ids) * m * (1 - (xs - lmbd) / (1 - lmbd))
    return(valz)
  }
  
  f <- function(x) {
    3 * zigzag(x, k, m, lmbd) * abs(log(abs(x) * 1000 + 1)) + 30 - 30 * abs(cos(x / (pi * 10)))
  }
  
  y <- sum(sapply(x, f))
  
  return(y)
}

get_xl <- function(nx) {
  rep(-100, nx)
}

get_xu <- function(nx) {
  rep(100, nx)
}

get_fmin <- function(nx) {
  0
}

get_xmin <- function(nx) {
  rep(0, nx)
}
