# Перевод функции ABS_3 из MATLAB в R
ABS_3 <- function(x = NULL) {
  # Параметры функции, если x не предоставлен
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
  k <- 2
  lmbd <- 0.99
  
  # Вспомогательная функция zigzag
  zigzag <- function(xs, k, m, lmbd) {
    xs <- abs(xs)
    xs <- xs / k - floor(xs / k)
    ids <- xs <= lmbd
    valz <- 1 - m + ids * m * (xs / lmbd) + (1 - ids) * m * (1 - (xs - lmbd) / (1 - lmbd))
    return(valz)
  }
  
  # Основная функция
  f <- function(x) {
    10 * abs(sin(0.1 * x)) + 3 * 10^(-9) * zigzag(x, k, m, lmbd) * abs((x - 40) * (x - 185) * x * (x + 50) * (x + 180))
  }
  
  # Расчет значения функции
  y <- sum(sapply(x, f))
  return(y)
}

# Вспомогательные функции для получения границ, минимального значения и минимальной точки
get_xl <- function(nx) rep(-100, nx)
get_xu <- function(nx) rep(100, nx)
get_fmin <- function(nx) 0
get_xmin <- function(nx) rep(0, nx)
# Пример использования
x <- c(1, 2, 3)
print(ABS_3(x))
