Layeb03 <- function(x) {
  # -------------------------------------------------------------------------
  # MATLAB coding by: Linas Stripinis
  # Name:
  #   Layeb03.m
  #
  # References:
  #  - A. Layeb.  (2022): New hard benchmark functions for global optimization. 
  #    doi:10.48550/ARXIV.2202.04606. URL: https://arxiv.org/abs/2202.04606
  #
  # Глобально оптимальное решение:
  #   f = -n + 1
  #   x(i) = pi, i = 1,...,n
  #
  # Границы переменных по умолчанию:
  #   -10 <= x(i) <= 10, i = 1,...,n
  #   
  # Свойства задачи:
  #   n  = "n >= 2";
  #   #g = 0;
  #   #h = 0;
  #
  # Известные характеристики тестовой функции:
  #   Дифференцируемая, несепарабельная, масштабируемая, многомодальная,
  #   не выпуклая, без плато, ненулевое решение, асимметричная
  # -------------------------------------------------------------------------
  
  # Проверка на отсутствие входных данных (если функция вызывается без аргументов)
  if (missing(x)) {
    # Создание структуры, описывающей функцию, и возвращение ее
    y$n <- 0  # Количество переменных (размерность пространства поиска)
    y$ng <- 0  # Количество ограничений типа равенства
    y$nh <- 0  # Количество ограничений типа неравенства
    y$xl <- function(nx) rep(-10, nx)  # Функция для задания нижних границ переменных
    y$xu <- function(nx) rep(10, nx)   # Функция для задания верхних границ переменных
    y$fmin <- function(nx) 0            # Функция для задания глобального минимума
    y$xmin <- function(nx) rep(pi, nx)  # Функция для задания точки, в которой достигается минимум
    y$features <- c(1, 0, 1, 1, 0, 0, 0, 0)  # Вектор, описывающий свойства задачи
    y$libraries <-
      