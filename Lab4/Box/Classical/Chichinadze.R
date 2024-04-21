Chichinadze <- function(x) {
  # -------------------------------------------------------------------------
  # MATLAB кодирование: Linas Stripinis
  # Название:
  #   Chichinadze.R
  #
  # Ссылки:
  #  - Гавана, А.: Глобальные бенчмарки и ампго. 
  #    URL: http://infinity77.net/global_optimization/index.html	
  #  - Момин Джамил и Син-Ши Ян, Обзор литературы по бенчмарк-функциям для
  #    глобальных задач оптимизации, Int. Journal of Mathematical Modelling and
  #    Numerical Optimisation, том 4, № 2, с. 150–194 (2013).
  #    DOI: 10.1504/IJMMNO.2013.055204
  #  - Роди Олденхуйс (2020): Тестовые функции для алгоритмов глобальной оптимизации
  #    URL: https://github.com/rodyo/FEX-testfunctions/releases/tag/v1.5
  #
  # Глобально оптимальное решение:
  #   f = -42.94438701899099442016
  #   x = c(6.18986658601429873272, 0.49999992290493755487)
  #
  # Предопределенные границы переменных:
  #   -30 <= x[i] <= 30, i = 1,...,n
  #
  # Свойства задачи:
  #   n  = 2;
  #   #g = 0;
  #   #h = 0;
  #
  # Известные характеристики тестовой функции:
  #   Дифференцируемая, Сепарабельная, Не-масштабируемая, Многомодальная,
  #   Не-выпуклая, Не-плато, Не-нулевое-решение, Асимметричная
  # -------------------------------------------------------------------------
  if (missing(x)) {
    return(list(nx = 2,
                ng = 0,
                nh = 0,
                xl = function(nx) get_xl(nx),
                xu = function(nx) get_xu(nx),
                fmin = function(nx) get_fmin(nx),
                xmin = function(nx) get_xmin(nx),
                features = c(1, 1, 0, 1, 0, 0, 0, 0),
                libraries = c(0, 0, 0, 1, 1, 0, 1, 0, 0, 0)))
  }
  

  
  y <- x[1]^2 - 12*x[1] + 11 + 10*cos(pi*x[1]/2) + 8*sin(5*pi*x[1]/2) -
    sqrt(1/5)*exp(-0.5*((x[2] - 0.5)^2))
  return(y)
}

get_xl <- function(nx) {
  return(rep(-30, nx))
}

get_xu <- function(nx) {
  return(rep(30, nx))
}

get_fmin <- function(nx) {
  return(-42.94438701899099442016)
}

get_xmin <- function(nx) {
  return(c(6.18986658601429873272, 0.49999992290493755487))
}
