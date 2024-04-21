# Определение функции DixonAndPrice
# Параметры:
#   x: Входной вектор
# Возвращает:
#   Значение функции DixonAndPrice в заданной точке
DixonAndPrice <- function(x) {
  # Проверка на отсутствие входных данных (для получения свойств функции)
  if (missing(x)) {
    return(list(nx = 0,  # Количество измерений
                ng = 0,  # Количество неравенств
                nh = 0,  # Количество равенств
                xl = function(nx) get_xl(nx),  # Нижние границы
                xu = function(nx) get_xu(nx),  # Верхние границы
                fmin = function(nx) get_fmin(nx),  # Глобальный минимум
                xmin = function(nx) get_xmin(nx),  # Точка глобального минимума
                features = c(1, 0, 1, 1, 0, 0, 0, 0),  # Характеристики функции
                libraries = c(1, 1, 0, 1, 1, 0, 0, 0, 0, 0)))  # Связанные библиотеки
  }
  
  # Вычисление значения функции DixonAndPrice
  n <- length(x)
  s1 <- 0
  for (j in 2:n) {
    s1 <- s1 + j * (2 * x[j]^2 - x[j - 1])^2
  }
  y <- s1 + (x[1] - 1)^2
  
  return(y)
}

# Определение функции для получения нижних границ
get_xl <- function(nx) {
  return(rep(-10, nx))
}

# Определение функции для получения верхних границ
get_xu <- function(nx) {
  return(rep(10, nx))
}

# Определение функции для получения глобального минимума
get_fmin <- function(nx) {
  return(0)
}

# Определение функции для получения точки глобального минимума
get_xmin <- function(nx) {
  xmin <- numeric(nx)
  for (i in 1:nx) {
    xmin[i] <- 2^(-((2^i - 2)/(2^i)))
  }
  return(xmin)
}
