Cigar <- function(x) {
  # -------------------------------------------------------------------------
  # MATLAB кодирование: Linas Stripinis
  # Название:
  #   Cigar.R
  #
  # Ссылки:
  #  - Гавана, А.: Глобальные бенчмарки и ампго. 
  #    URL: http://infinity77.net/global_optimization/index.html	
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
  #   Дифференцируемая, Не-сепарабельная, Масштабируемая, Унимодальная,
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
                features = c(1, 0, 1, 0, 0, 0, 1, 0),
                libraries = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0)))
  }
  

  
  y <- x[1]^2 + 10^6 * sum(x^2)
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
