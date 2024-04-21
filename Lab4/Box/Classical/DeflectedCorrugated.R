# Определение функции DeflectedCorrugated
# Параметры:
#   x: Входной вектор
# Возвращает:
#   Значение функции DeflectedCorrugated в заданной точке
DeflectedCorrugated <- function(x) {
  # Проверка на отсутствие входных данных (для получения свойств функции)
  if (missing(x)) {
    return(list(nx = 0,  # Количество измерений
                ng = 0,  # Количество неравенств
                nh = 0,  # Количество равенств
                xl = function(nx) get_xl(nx),  # Нижние границы
                xu = function(nx) get_xu(nx),  # Верхние границы
                fmin = function(nx) get_fmin(nx),  # Глобальный минимум
                xmin = function(nx) get_xmin(nx),  # Точка глобального минимума
                features = c(1, 0, 1, 1, 0, 0, 0, 1),  # Характеристики функции
                libraries = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0)))  # Связанные библиотеки
  }
  
  # Вычисление значения функции DeflectedCorrugated
  alpha <- 5
  k <- 5
  y <- 0.1 * sum((x - alpha)^2) - cos(k * sqrt(sum((x - alpha)^2)))
  
  return(y)
}

# Определение функции для получения нижних границ
get_xl <- function(nx) {
  return(rep(0, nx))
}

# Определение функции для получения верхних границ
get_xu <- function(nx) {
  return(rep(10, nx))
}

# Определение функции для получения глобального минимума
get_fmin <- function(nx) {
  return(-1)
}

# Определение функции для получения точки глобального минимума
get_xmin <- function(nx) {
  return(rep(5, nx))
}
