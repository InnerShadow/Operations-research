# Определение функции GallagherGaussian101BBOB
GallagherGaussian101BBOB <- function(x) {
  # Проверка, были ли переданы аргументы в функцию
  if (missing(x)) {
    # Если аргументы не переданы, возвращаем информацию о задаче
    nx <- 0
    ng <- 0
    nh <- 0
    xl <- function(nx) get_xl(nx)
    xu <- function(nx) get_xu(nx)
    fmin <- function(nx) get_fmin(nx)
    xmin <- function(nx) get_xmin(nx)
    features <- c(1, 0, 1, 1, 0, 0, 0, 0)
    libraries <- c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
    return(list(nx = nx, ng = ng, nh = nh, xl = xl, xu = xu, fmin = fmin, xmin = xmin, features = features, libraries = libraries))
  }
  
  # Проверка, нужно ли транспонировать переданную матрицу x

  
  # Проверка наличия или необходимости пересчета некоторых переменных
  dim <- length(x)
  if (!exists("fopt") || dim != length(x) || !exists("funid") || funid != 21 || is.null(xlocal) || is.null(nhighpeaks) || is.null(peakvalues) || is.null(fac) || is.null(arrscales) || is.null(rotation)) {
    fopt <- get_fmin(dim)
    funid <- 21
    fitvalues <- c(1.1, 9.1)
    nhighpeaks <- 101
    fac2 <- 1
    highpeakcond <- sqrt(1000)
    maxcondition <- 1000
    rotation <- compute_rotation(funid, dim)
    arrcondition <- maxcondition^seq(0, 1, length.out = nhighpeaks - 1)
    idx <- argsort(runif(nhighpeaks - 1, funid))
    arrcondition <- c(highpeakcond, arrcondition[idx])
    arrscales <- matrix(0, nrow = nhighpeaks, ncol = dim)
    for (i in 1:length(arrcondition)) {
      s <- arrcondition[i]^(seq(-0.5, 0.5, length.out = dim))
      idx <- argsort(runif(dim, funid + (1e+3)*(i - 1)))
      arrscales[i, ] <- s[idx]
    }
    peakvalues <- c(10, seq(fitvalues[1], fitvalues[2], length.out = nhighpeaks - 1))
    fac <- -0.5/dim
    xlocal <- fac2 * matrix(10 * runif(dim * nhighpeaks, funid) - 5, nrow = dim, ncol = nhighpeaks) %*% rotation
    xlocal[1, ] <- 0.8 * xlocal[1, ]
  }
  
  # Вычисление значения функции
  z <- t(x) %*% rotation
  xx <- matrix(rep(z, each = nhighpeaks), nrow = dim) - xlocal
  f <- peakvalues * exp(fac * rowSums(arrscales * xx^2))
  y <- toz(10 - pmax(f, -1))^2 + pen(x) + fopt
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
  z <- sum(pmax(0, abs(x) - 5)^2 * sign(x))
  return(z)
}

# Функция преобразования
toz <- function(x) {
  z <- x
  for (i in seq_along(x)) {
    if (x[i] != 0) {
      xx <- log(abs(x[i]))
    } else {
      xx <- 0
    }
    if (x[i] > 0) {
      c1 <- 10
    } else {
      c1 <- 5.5
    }
    if (x[i] > 0) {
      c2 <- 7.9
    } else {
      c2 <- 3.1
    }
    z[i] <- sign(x[i]) * exp(xx + 0.049 * (sin(c1 * xx) + sin(c2 * xx)))
  }
  return(z)
}

# Функция вычисления матрицы поворота
compute_rotation <- function(seed, dim) {
  B <- t(matrix(gauss(dim * dim, seed), nrow = dim))
  for (i in 1:dim) {
    for (j in 1:(i - 1)) {
      B[i, ] <- B[i, ] - sum(B[i, ] * B[j, ]) * B[j, ]
    }
    B[i, ] <- B[i, ] / sqrt(sum(B[i, ]^2))
  }
  return(B)
}

# Функция генерации случайных чисел по нормальному закону распределения
gauss <- function(N, seed) {
  r <- unif(2 * N, seed)
  g <- sqrt(-2 * log(r[seq_len(N)])) * cos(2 * pi * r[(N + 1):(2 * N)])
  g[g == 0] <- 1e-99
  return(g)
}

# Функция генерации случайных чисел по равномерному закону распределения
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
}
  
  