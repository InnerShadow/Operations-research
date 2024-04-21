Colville <- function(x) {
  # -------------------------------------------------------------------------
  # MATLAB кодирование: Linas Stripinis
  # Название:
  #   Colville.R
  #
  # Ссылки:
  #  - Хедар, А. (2005): Тестовые функции для безусловной глобальной оптимизации. 
  #    URL: http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO.htm																				
  #  - Сурянович, С., Бингем, Д. (2013): Виртуальная библиотека симуляционных
  #    экспериментов: Тестовые функции и наборы данных. 
  #    URL: http://www.sfu.ca/~ssurjano/index.html	
  #  - Гавана, А.: Глобальные бенчмарки и ампго. 
  #    URL: http://infinity77.net/global_optimization/index.html	
  #  - Момин Джамил и Син-Ши Ян, Обзор литературы по бенчмарк-функциям для
  #    глобальных задач оптимизации, Int. Journal of Mathematical Modelling and
  #    Numerical Optimisation, том 4, № 2, с. 150–194 (2013).
  #    DOI: 10.1504/IJMMNO.2013.055204
  #  - Роди Олденхуис (2020): Тестовые функции для алгоритмов глобальной оптимизации
  #    URL: https://github.com/rodyo/FEX-testfunctions/releases/tag/v1.5
  #
  # Глобально оптимальное решение:
  #   f = 0
  #   x = [1; 1; 1; 1]
  #
  # Предопределенные границы переменных:
  #   -10 <= x[i] <= 10, i = 1,...,n
  #   
  # Свойства задачи:
  #   n  = 4;
  #   #g = 0;
  #   #h = 0;
  #
  # Известные характеристики тестовой функции:
  #   Дифференцируемая, Не-сепарабельная, Не-масштабируемая, Унимодальная,
  #   Выпуклая, Не-плато, Не-нулевое-решение, Асимметричная
  # -------------------------------------------------------------------------
  if (missing(x)) {
    return(list(nx = 4,
                ng = 0,
                nh = 0,
                xl = function(nx) get_xl(nx),
                xu = function(nx) get_xu(nx),
                fmin = function(nx) get_fmin(nx),
                xmin = function(nx) get_xmin(nx),
                features = c(1, 0, 0, 0, 1, 0, 0, 0),
                libraries = c(1, 1, 0, 1, 1, 0, 1, 0, 0, 0)))
  }
  

  
  y <- 100 * (x[1]^2 - x[2])^2 + (x[1] - 1)^2 + (x[3] - 1)^2 + 90 * (x[3]^2 -
                                                                       x[4])^2 + 10.1 * ((x[2] - 1)^2 + (x[4] - 1)^2) + 19.8 * (x[2] -
                                                                                                                                  1) * (x[4] - 1)
  
  return(y)
}

get_xl <- function(nx) {
  return(rep(-10, nx))
}

get_xu <- function(nx) {
  return(rep(10, nx))
}

get_fmin <- function(nx) {
  return(0)
}

get_xmin <- function(nx) {
  return(rep(1, nx))
}
