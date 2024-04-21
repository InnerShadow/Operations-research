ABS_7 <- function(x = NULL) {
  # Если аргументы не предоставлены, возвращаем параметры функции
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
  
  # Проверка на ориентацию вектора

  
  m <- 0.1000
  k <- 16
  lmbd <- 0.1000
  
  f <- function(x) {
    zigzag(x, k, m, lmbd) * 3 * (abs(log(abs(x) * 1000 + 1))) + 30 - 30 * abs(cos(x / (pi * 10)))
  }
  
  y <- sum(f(f(x)))
  
  return(y)
}

zigzag <- function(xs, k, m, lmbd) {
  xs <- abs(xs)
  xs <- xs / k - floor(xs / k)
  ids <- xs <= lmbd
  valz <- 1 - m + ifelse(ids, m * (xs / lmbd), m * (1 - (xs - lmbd) / (1 - lmbd)))
  return(valz)
}


