ChungR <- function(x) {
  # -------------------------------------------------------------------------
  # MATLAB кодирование: Linas Stripinis
  # Название:
  #   ChungR.R
  #
  # Ссылки:
  #  - Момин Джамил и Син-Ши Ян, Обзор литературы по бенчмарк-функциям для
  #    глобальных задач оптимизации, Int. Journal of Mathematical Modelling and
  #    Numerical Optimisation, том 4, № 2, с. 150–194 (2013).
  #    DOI: 10.1504/IJMMNO.2013.055204
  #
  # Глобально оптимальное решение:
  #   f = 0
  #   x(i) = 0, i = 1,...,n
  #
  # Предопределенные границы переменных:
  #   -100 <= x[i] <= 100, i = 1,...,n
  #   
  # Свойства задачи:
  #   n  = любое измерение;
  #   #g = 0;
  #   #h = 0;
  #
  # Известные характеристики тестовой функции:
  #   Дифференцируемая, Сепарабельная, Масштабируемая, Унимодальная,
  #   Выпуклая, Не-плато, Нулевое-решение, Симметричная
  # -------------------------------------------------------------------------
  if (missing(x)) {
    return(list(nx = 0,
                ng = 0,
                nh = 0,
                xl = function(nx) get_xl(nx),
                xu = function(nx) get_xu(nx),
                fmin = function(nx) get_fmin(nx),
                xmin = function(nx) get_xmin(nx),
                features = c(1, 1, 1, 0, 1, 0, 1, 1),
                libraries = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)))
  }
  

  
  y <- sum(x^2)^2
  return(y)
}

get_xl <- function(nx) {
  return(rep(-100, nx))
}

get_xu <- function(nx) {
  return(rep(100, nx))
}

get_fmin <- function(nx) {
  return(0)
}

get_xmin <- function(nx) {
  return(rep(0, nx))
}
