# Функция вычисления значения целевой функции SphereBBOB
SphereBBOB <- function(x) {
  # Если функция вызывается без аргументов, возвращаем структуру с информацией о функции
  if (missing(x)) {
    return(list(
      nx = 0,
      ng = 0,
      nh = 0,
      xl = function(nx) get_xl(nx),
      xu = function(nx) get_xu(nx),
      fmin = function(nx) get_fmin(nx),
      xmin = function(nx) get_xmin(nx),
      features = c(1, 1, 1, 0, 1, 0, 0, 0),
      libraries = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
    ))
  }
  
  # Переносим вектор-строку x в вектор-столбец, если необходимо

  
  # Инициализация переменных xopt, dim, fopt и funid
  if (is.null(dim) || is.null(fopt) || is.null(xopt) || dim(x) != dim || funid != 1) {
    dim <<- dim(x)
    xopt <<- get_xmin(dim)
    fopt <<- get_fmin(dim)
    funid <<- 1
  }
  
  # Сдвигаем x на xopt
  z <- x_shift(x, xopt)
  
  # Вычисляем значение функции SphereBBOB
  y <- sum(z^2) + fopt
  return(y)
}

# Функция возвращающая нижние границы переменных
get_xl <- function(nx) {
  return(rep(-5, nx))
}

# Функция возвращающая верхние границы переменных
get_xu <- function(nx) {
  return(rep(5, nx))
}

# Функция сдвига вектора x на xopt
x_shift <- function(x, xopt) {
  return(x - xopt)
}

# Функция генерации N случайных чисел из нормального распределения
gauss <- function(N, seed) {
  r <- unif(2*N, seed)
  g <- sqrt(-2*log(r[1:N])) * cos(2*pi*r[(N+1):(2*N)])
  g[g == 0] <- 1e-99
  return(g)
}

# Функция генерации N случайных чисел из равномерного распределения
unif <- function(N, inseed) {
  inseed <- abs(inseed)
  if (inseed < 1) {
    inseed <- 1
  }
  rgrand <- rep(0, 32)
  aktseed <- inseed
  
  for (i in 39:0) {
    tmp <- floor(aktseed / 127773)
    aktseed <- 16807 * (aktseed - tmp * 127773) - 2836 * tmp
    if (aktseed < 0) {
      aktseed <- aktseed + 2147483647
    }
    if (i < 32) {
      rgrand[i + 1] <- aktseed
    }
  }
  
  aktrand <- rgrand[1]
  r <- rep(0, N)
  
  for (i in 1:N) {
    tmp <- floor(aktseed / 127773)
    aktseed <- 16807 * (aktseed - tmp * 127773) - 2836 * tmp
    if (aktseed < 0) {
      aktseed <- aktseed + 2147483647
    }
    tmp <- floor(aktrand / 67108865) + 1
    aktrand <- rgrand[tmp]
    rgrand[tmp] <- aktseed
    r[i] <- aktrand / 2147483647
  }
  
  r[r == 0] <- 1e-15
  return(r)
}

# Функция возвращает минимальное значение функции
get_fmin <- function() {
  r <- gauss(1, 1) / gauss(1, 1 + 1)
  fmin <- min(1000, max(-1000, round(100 * 100 * r) / 100))
  return(fmin)
}

# Функция возвращает начальные значения переменных
get_xmin <- function(nx) {
  r <- unif(nx, 1)
  xmin <- 8 * floor(1e+4 * r) / 1e+4 - 4
  xmin[xmin == 0] <- -1e-5
  return(xmin)
}
