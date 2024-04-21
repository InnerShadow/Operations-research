GriewankRosenbrockBBOB <- function(x) {
  # Проверка на количество аргументов
  if (missing(x)) {
    y <- list(
      nx = 0,
      ng = 0,
      nh = 0,
      xl = function(nx) get_xl(nx),
      xu = function(nx) get_xu(nx),
      fmin = function(nx) get_fmin(nx),
      xmin = function(nx) get_xmin(nx),
      features = c(1, 0, 1, 1, 0, 0, 0, 0),
      libraries = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
    )
    return(y)
  }
  
  # Если матрица, преобразуем в вектор

  
  # Инициализация параметров
  dim <- length(x)
  xopt <- get_xmin(dim)
  fopt <- get_fmin(dim)
  funid <- 19
  linearTF <- max(c(1, sqrt(dim) / 8)) * compute_rotation(19, dim)
  
  # Вычисление функции
  z <- t(x) %*% linearTF + 0.5
  s <- 100 * (z[-length(z)]^2 - z[-1])^2 + (1 - z[-length(z)])^2
  y <- 10 + 10 * sum(s / 4000 - cos(s)) / (dim - 1) + fopt + pen(x)
  
  return(y)
}

# Функция получения нижних границ переменных
get_xl <- function(nx) {
  return(rep(-5, nx))
}

# Функция получения верхних границ переменных
get_xu <- function(nx) {
  return(rep(5, nx))
}

# Функция штрафа
pen <- function(x) {
  return(sum((pmax(0, (abs(x) - 5)))^2))
}

# Функция вычисления вращения
compute_rotation <- function(seed, dim) {
  B <- matrix(gauss(dim * dim, seed), nrow = dim, ncol = dim)
  for (i in 1:dim) {
    for (j in 1:(i - 1)) {
      B[i, ] <- B[i, ] - sum(B[i, ] * B[j, ]) * B[j, ]
    }
    B[i, ] <- B[i, ] / sqrt(sum(B[i, ]^2))
  }
  return(B)
}

# Функция генерации случайных чисел из распределения Гаусса
gauss <- function(N, seed) {
  r <- unif(2 * N, seed)
  g <- sqrt(-2 * log(r[1:N])) * cos(2 * pi * r[(N + 1):(2 * N)])
  g[g == 0] <- 1e-99
  return(g)
}

# Функция генерации равномерно распределенных случайных чисел
unif <- function(N, inseed) {
  inseed <- abs(inseed)
  if (inseed < 1) {
    inseed <- 1
  }
  rgrand <- numeric(32)
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
  r <- numeric(N)
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

# Функция минимального значения
get_fmin <- function(nx) {
  fmin <- min(1000, max(-1000, (round(100 * 100 * gauss(1, 19) / gauss(1, 19 + 1)) / 100)))
  return(fmin)
}

# Функция минимального значения переменных
get_xmin <- function(nx) {
  scale <- max(1, sqrt(nx) / 8)
  linearTF <- scale * compute_rotation(19, nx)
  xmin <- (0.5 * rep(1, nx)) / linearTF
  return(xmin)
}

