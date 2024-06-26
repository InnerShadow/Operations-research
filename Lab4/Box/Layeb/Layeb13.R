Layeb13 <- function(x) {
  # -------------------------------------------------------------------------
  # MATLAB coding by: Linas Stripinis
  # Name:
  #   Layeb13.m
  #
  # References:
  #  - A. Layeb.  (2022): New hard benchmark functions for global optimization. 
  #    doi:10.48550/ARXIV.2202.04606. URL: https://arxiv.org/abs/2202.04606
  #
  # Глобально оптимальное решение:
  #   f = 0
  #   x = чередующиеся значения pi/4 и -pi/4
  #
  # Границы переменных по умолчанию:
  #   -5 <= x(i) <= 5, i = 1,...,n
  #   
  # Свойства задачи:
  #   n  = "n >= 2";
  #   #g = 0;
  #   #h = 0;
  #
  # Известные характеристики тестовой функции:
  #   Дифференцируемая, несепарабельная, масштабируемая, многомодальная,
  #   не выпуклая, без плато, ненулевое решение, симметричная
  # -------------------------------------------------------------------------
  
  # Проверка на отсутствие входных данных (если функция вызывается без аргументов)
  if (missing(x)) {
    # Создание структуры, описывающей функцию, и возвращение ее
    y$n <- 0  # Количество переменных (размерность пространства поиска)
    y$ng <- 0  # Количество ограничений типа равенства
    y$nh <- 0  # Количество ограничений типа неравенства
    y$xl <- function(nx) rep(-5, nx)  # Функция для задания нижних границ переменных
    y$xu <- function(nx) rep(5, nx)   # Функция для задания верхних границ переменных
    y$fmin <- function(nx) 0  # Функция для задания глобального минимума
    y$xmin <- function(nx) {  # Функция для задания точки, в которой достигается минимум
      xmin <- rep(pi/4, nx)
      for (i in seq_len(nx)) {
        if (i %% 2 == 0) {
          xmin[i] <- -pi/4
        }
      }
      return(xmin)
    }
    y$features <- c(1, 0, 1, 1, 0, 0, 0, 1)  # Вектор, описывающий свойства задачи
    y$libraries <- c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0)  # Вектор, указывающий на использование библиотек
    return(y)  # Возвращение структуры
    end
    
    # Инициализация переменной для хранения значения функции
    y <- 0
    
    # Получение размерности вектора переменных
    dim <- length(x)
    
    # Цикл по переменным
    for (i in 1:(dim - 1)) {
      # Расчет значения функции
      y <- y + abs(cos(x[i] - x[i + 1])) + 100*abs(log(abs(x[i] + x[i + 1]) + 1))^0.1
    }
    
    # Возвращение значения функции
    return(y)
  }
}