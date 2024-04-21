# Перевод функции ABS_8 из MATLAB в R
ABS_8 <- function(x = NULL) {
  if (is.null(x)) {
    # Возвращаем список свойств, если x не предоставлен
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
  

  
  m <- 0.9000
  k <- 4
  lmbd <- 0.0100
  
  f <- function(x) {
    zigzag <- function(xs, k, m, lmbd) {
      xs <- abs(xs)
      xs <- xs / k - floor(xs / k)
      ids <- xs <= lmbd
      1 - m + ids * m * (xs / lmbd) + (!ids) * m * (1 - (xs - lmbd) / (1 - lmbd))
    }
    
    zigzag_value <- zigzag(x, k, m, lmbd)
    3 * (abs(log(abs(x) * 1000 + 1))) * zigzag_value + 30 - 30 * abs(cos(x / (pi * 10)))
  }
  
  y <- sum(f(f(x)))
  return(y)
}

