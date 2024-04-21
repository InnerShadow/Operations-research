Вот перевод данного кода MATLAB на R с комментариями:
  
  ```r
# Функция GallagherGaussian21BBOB
GallagherGaussian21BBOB <- function(x) {
  # Проверка аргументов
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
  
  # Преобразование вектора x, если необходимо

  
  # Инициализация переменных при первом запуске
  if (!exists("dim") || dim != length(x) || funid != 22 || is.null(xlocal) ||
      is.null(nhighpeaks) || is.null(peakvalues) || is.null(fac) ||
      is.null(arrscales) || is.null(rotation)) {
    dim <<- length(x)
    fopt <<- get_fmin(dim)
    funid <<- 22
    fitvalues <- c(1.1, 9.1)
    nhighpeaks <<- 21
    fac2 <- 0.98
    highpeakcond <- 1000
    maxcondition <- 1000
    rotation <<- compute_rotation(funid, dim)
    arrcondition <- (maxcondition^seq(0, 1, length.out = nhighpeaks - 1))
    idx <- argsort(runif(nhighpeaks - 1, funid))
    arrcondition <- c(highpeakcond, arrcondition[idx])
    arrscales <- matrix(0, nrow = nhighpeaks, ncol = dim)
    for (i in 1:length(arrcondition)) {
      s <- arrcondition[i]^seq(-0.5, 0.5, length.out = dim)
      idx <- argsort(runif(dim, funid + (1e+3) * (i - 1)))
      arrscales[i, ] <- s[idx]
    }
    peakvalues <- c(10, seq(fitvalues[1], fitvalues[2], length.out = nhighpeaks - 1))
    fac <<- -0.5 / dim
    xlocal <<- fac2 * matrix(10 * runif(dim * nhighpeaks, funid) - 5, ncol = dim) %*% rotation
    xlocal[1, ] <- 0.8 * xlocal[1, ]
  }
  
  z <- t(x) %*% rotation
  xx <- (matrix(rep(z, nhighpeaks), ncol = nhighpeaks) - xlocal)
  f <- (peakvalues * exp(fac * rowSums(arrscales * xx^2)))
  y <- sum(toz(10 - max(c(max(f), -1)))^2) + pen(x) + fopt
  return(y)
}

# Вспомогательные функции

# Функция get_xl
get_xl <- function(nx) {
  return(rep(-5, nx))
}

# Функция get_xu
get_xu <- function(nx) {
  return(rep(5, nx))
}

# Функция pen
pen <- function(x) {
  return(sum(pmax(0, (abs(x) - 5))^2))
}

# Функция toz
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

# Функция compute_rotation
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

# Функция gauss
gauss <- function(N, seed) {
  r <- unif(2 * N, seed)
  g <- sqrt(-2 * log(r[1:N])) * cos(2 * pi * r[(N + 1):(2 * N)])
  g[g == 0] <- 1e-99
  return(g)
}

# Функция unif
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

# Функция argsort
argsort <- function(M, mode = "ascend") {
  
  