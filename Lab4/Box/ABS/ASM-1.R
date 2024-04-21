

# Основная функция
ABS_1 <- function(x) {
  if (missing(x)) {
    return(list(
      nx = 0,
      ng = 0,
      nh = 0,
      xl = function(nx) get_xl(nx),
      xu = function(nx) get_xu(nx),
      fmin = function(nx) get_fmin(nx),
      xmin = function(nx) get_xmin(nx),
      features = c(0, 1, 1, 1, 0, 0, 1, 1),
      libraries = c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0)
    ))
  }
  

  
  m <- 1; k <- 16; lmbd <- 0.0100
  
  f <- function(x) {
    10 * abs(sin(0.1 * x)) + 3e-9 * zigzag(x, k, m, lmbd) * abs((x - 40) * (x - 185) * x * (x + 50) * (x + 180))
  }
  
  y <- sum(apply(x, 1, f))
  
  return(y)
}

# Вспомогательные функции
zigzag <- function(xs, k, m, lmbd) {
  xs <- abs(xs)
  xs <- xs / k - floor(xs / k)
  ids <- xs <= lmbd
  1 - m + ids * m * (xs / lmbd) + (!ids) * m * (1 - (xs - lmbd) / (1 - lmbd))
}

get_xl <- function(nx) {
  -100 * rep(1, nx)
}

get_xu <- function(nx) {
  100 * rep(1, nx)
}

get_fmin <- function(nx) {
  0
}

get_xmin <- function(nx) {
  rep(0, nx)
}

# Пример использования
x <- matrix(c(-90, 50), ncol = 1)
result <- ABS_1(x)
print(result)
