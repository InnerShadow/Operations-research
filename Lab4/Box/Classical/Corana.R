Corana <- function(x) {
  # -------------------------------------------------------------------------
  # MATLAB кодирование: Linas Stripinis
  # Название:
  #   Corana.R
  #
  # Ссылки:
  #  - Гавана, А.: Глобальные бенчмарки и ампго. 
  #    URL: http://infinity77.net/global_optimization/index.html	
  #  - Момин Джамил и Син-Ши Ян, Обзор литературы по бенчмарк-функциям для
  #    глобальных задач оптимизации, Int. Journal of Mathematical Modelling and
  #    Numerical Optimisation, том 4, № 2, с. 150–194 (2013).
  #    DOI: 10.1504/IJMMNO.2013.055204
  #
  # Глобально оптимальное решение:
  #   f = 0
  #   x[i] = 0, i = 1,...,n
  #
  # Предопределенные границы переменных:
  #   -5 <= x[i] <= 5, i = 1,...,n
  #
  # Свойства задачи:
  #   n  = any dimension;
  #   #g = 0;
  #   #h = 0;
  #
  # Известные характеристики тестовой функции:
  #   Не-дифференцируемая, Сепарабельная, Масштабируемая, Многомодальная,
  #   Не-выпуклая, Не-плато, Нулевое-решение, Асимметричная
  # -------------------------------------------------------------------------
  if (missing(x)) {
    return(list(nx = 0,
                ng = 0,
                nh = 0,
                xl = function(nx) get_xl(nx),
                xu = function(nx) get_xu(nx),
                fmin = function(nx) get_fmin(nx),
                xmin = function(nx) get_xmin(nx),
                features = c(0, 1, 1, 1, 0, 0, 1, 0),
                libraries = c(0, 0, 0, 1, 1, 0, 0, 0, 0, 0)))
  }
  

  
  s <- 0.2
  d <- matrix(c(1, 1000, 10, 100), nrow = 1, ncol = 4 + floor(length(x)/4))
  y <- 0
  for (i in 1:length(x)) {
    z <- 0.2 * (abs(x[i]/s) + 0.49999) * sign(x[i])
    if (x[i] - z < 0.05) {
      y <- y + 0.15 * d[i] * (z - 0.05 * sign(z))^2
    } else {
      y <- y + d[i] * x[i]^2
    }
  }
  
  return(y)
}

get_xl <- function(nx) {
  return(rep(-5, nx))
}

get_xu <- function(nx) {
  return(rep(5, nx))
}

get_fmin <- function(nx) {
  return(0)
}

get_xmin <- function(nx) {
  return(rep(0, nx))
}
