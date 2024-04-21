# KatsuuraBBOB функция
KatsuuraBBOB <- function(x) {
  # Проверяем, если функция вызвана без аргументов, возвращаем структуру с информацией о задаче
  if (missing(x)) {
    return(list(
      nx = 0,
      ng = 0,
      nh = 0,
      xl = function(nx) get_xl(nx),
      xu = function(nx) get_xu(nx),
      fmin = function(nx) get_fmin(nx),
      xmin = function(nx) get_xmin(nx),
      features = c(1, 0, 1, 1, 0, 0, 0, 0),
      libraries = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
    ))
  }
  
  # Переводим вектор x в матрицу, если он в виде строки
  if (is.vector(x) && length(dim(x)) == 0) {
    x <- t(x)
  }
  
  # Инициализация переменных, если они пусты или изменились параметры
  if (!exists("xopt") || !exists("dim") || !exists("fopt") || !exists("funid") || !exists("linearTF") ||
      is.null(xopt) || is.null(dim) || is.null(fopt) || is.null(funid) || is.null(linearTF) ||
      dim != length(x) || funid != 23) {
    dim <<- length(x)
    xopt <<- get_xmin(dim)
    fopt <<- get_fmin(dim)
    funid <<- 23
    linearTF <<- (compute_rotation(23, dim) %*% diag(sqrt(100) ^ (0:1 / dim))) %*% compute_rotation(23 + 1e+6, dim)
  }
  
  z <- t(x_shift(t(x), xopt)) %*% linearTF
  y <- 1
  for (i in 1:dim) {
    aa <- 0
    for (k in 1:32) {
      aa <- aa + abs(2 ^ k * z[i] - round(2 ^ (k) * z[i])) / 2 ^ k
    }
    y <- y * ((1 + (i) * aa)^(10 / (dim^1.2)))
  }
  y <- y * (10 / (dim^2)) + fopt + pen(x) - 10 / (dim^2)
  
  return(y)
}

# Функция для получения нижних границ переменных
get_xl <- function(nx) {
  return(rep(-5, nx))
}

# Функция для получения верхних границ переменных
get_xu <- function(nx) {
  return(rep(5, nx))
}

# Функция штрафа
pen <- function(x) {
  return(sum((pmax(0, abs(x) - 5) * sign(x))^2))
}

# Смещение переменных
x_shift <- function(x, xopt) {
  return(x - xopt)
}

# Функция гауссовского распределения
gauss <- function(N, seed) {
  r <- unif(2 * N, seed)
  g <- sqrt(-2 * log(r[1:N])) * cos(2 * pi * r[(N + 1):(2 * N)])
  g[g == 0] <- 1e-99
  return(g)
}

# Функция для вычисления матрицы поворота
compute_rotation <- function(seed, dim) {
  B <- matrix(gauss(dim * dim, seed), nrow = dim)
  for (i in 1:dim) {
    for (j in 1:(i - 1)) {
      B[i, ] <- B[i, ] - sum(B[i, ] * B[j, ]) * B[j, ]
    }
    B[i, ] <- B[i, ] / sqrt(sum(B[i, ]^2))
  }
  return(B)
}

# Функция для получения минимального значения
get_fmin <- function(nx) {
  return(min(c(1000, max(-1000, (round(100 * 100 * gauss(1, 23) / gauss(1, 23 + 1)) / 100)))))
}

# Функция для получения начальных значений переменных
get_xmin <- function(nx) {
  xmin <- 8 * floor(1e+4 * unif(nx, 23 + 1e+6)) / 1e+4 - 4
  xmin[xmin == 0] <- -1e-5
  return(xmin)
}

# Функция для получения равномерного распределения
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

