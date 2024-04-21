ABS_2 <- function(x = NULL) {
  # Если аргумент не предоставлен, возвращаем свойства функции
  if (is.null(x)) {
    return(list(
      nx = 0,
      ng = 0,
      nh = 0,
      xl = function(nx) rep(-100, nx),
      xu = function(nx) rep(100, nx),
      fmin = function(nx) 0,
      xmin = function(nx) rep(0, nx),
      features = c(0, 1, 1, 1, 0, 0, 1, 1),
      libraries = c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0)
    ))
  }
  
  m <- 0.5000
  k <- 8
  lmbd <- 0.0100
  
  f <- function(x) {
    10 * abs(sin(0.1 * x)) + 3e-9 * zigzag(x, k, m, lmbd) * abs((x - 40) * (x - 185) * x * (x + 50) * (x + 180))
  }
  y <- sum(f(x))
  
  return(y)
}

zigzag <- function(xs, k, m, lmbd) {
  xs <- abs(xs)
  xs <- xs / k - floor(xs / k)
  ids <- xs <= lmbd
  valz <- (1 - m) + ids * m * (xs / lmbd) + (!ids) * m * (1 - (xs - lmbd) / (1 - lmbd))
  return(valz)
}


