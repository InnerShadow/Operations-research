# Функция StepEllipsoidalBBOB
StepEllipsoidalBBOB <- function(x) {
  # Проверка на входные аргументы
  if (missing(x)) {
    y <- list(
      nx = 0,
      ng = 0,
      nh = 0,
      xl = function(nx) get_xl(nx),
      xu = function(nx) get_xu(nx),
      fmin = function(nx) get_fmin(nx),
      xmin = function(nx) get_xmin(nx),
      features = c(1, 0, 1, 0, 0, 1, 0, 0),
      libraries = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
    )
    return(y)
  }
  
  # Проверка размера входного вектора x

  
  # Определение параметров функции
  dim <- length(x)
  if (!exists("xopt") || !exists("fopt") || !exists("funid") || !exists("rotation") || !exists("scales") || !exists("linearTF") ||
      is.null(xopt) || is.null(fopt) || is.null(funid) || is.null(rotation) || is.null(scales) || is.null(linearTF) ||
      dim != length(x) || funid != 7) {
    xopt <<- get_xmin(dim)
    fopt <<- get_fmin(dim)
    funid <<- 7
    rotation <<- compute_rotation(7 + 1e+6, dim)
    scales <<- (100^seq(0, 1, length.out = dim))
    linearTF <<- compute_rotation(7, dim) %*% diag(sqrt(100/10)^seq(0, 1, length.out = dim))
  }
  
  z <- t(x_shift(t(x), xopt)) %*% linearTF
  x1 <- abs(z[1])
  z <- t(zzz(z)) %*% rotation
  y <- 0.1 * max(c(x1 * 1.0e-4, sum(scales * (z^2)))) + pen(x) + fopt
  return(y)
}

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
  return(sum((pmax(0, (abs(x) - 5)) * sign(x))^2))
}

# Функция zzz
zzz <- function(x) {
  l <- rep(0, length(x))
  for (i in seq_along(x)) {
    if (abs(x[i]) > 0.5) {
      l[i] <- round(x[i])
    } else {
      l[i] <- round(10 * x[i]) / 10
    }
  }
  return(l)
}

# Функция compute_rotation
compute_rotation <- function(seed, dim) {
  B <- matrix(gauss(dim * dim, seed), nrow = dim, byrow = TRUE)
  for (i in 1:dim) {
    for (j in 1:(i - 1)) {
      B[i,] <- B[i,] - sum(B[i,] * B[j,]) * B[j,]
    }
    B[i,] <- B[i,] / sqrt(sum(B[i,]^2))
  }
  return(B)
}

# Функция x_shift
x_shift <- function(x, xopt) {
  return(x - xopt)
}

# Функция gauss
gauss <- function(N, seed) {
  r <- unif(2 * N, seed)
  g <- sqrt(-2 * log(r[seq_len(N)])) * cos(2 * pi * r[(N + 1):(2 * N)])
  g[g == 0] <- 1e-99
  return(g)
}

# Функция unif
unif <- function(N, inseed) {
  inseed <- abs(inseed)
  if (inseed < 1) inseed <- 1
  rgrand <- numeric(32)
  aktseed <- inseed
  for (i in 39:0) {
    tmp <- floor(aktseed / 127773)
    aktseed <- 16807 * (aktseed - tmp * 127773) - 2836 * tmp
    if (aktseed < 0) aktseed <- aktseed + 2147483647
    if (i < 32) rgrand[i + 1] <- aktseed
  }
  aktrand <- rgrand[1]
  r <- numeric(N)
  for (i in seq_len(N)) {
    tmp <- floor(aktseed / 127773)
    aktseed <- 16807 * (aktseed - tmp * 127773) - 2836 * tmp
    if (aktseed < 0) aktseed <- aktseed + 2147483647
    tmp <- floor(aktrand / 67108865) + 1
    aktrand <- rgrand[tmp]
    rgrand[tmp] <- aktseed
    r[i] <- aktrand / 2147483647
  }
  r[r == 0] <- 1e-15
  return(r)
}

# Функция get_fmin
get_fmin <- function(nx) {
  return(min(1000, max(-1000, round(100 * 100 * gauss(1, 7) / gauss(1, 7 + 1)) / 100)))
}

# Функция get_xmin
get_xmin <- function(nx) {
  xmin <- 8 * floor(1e+4 * unif(nx, 7)) / 1e+4 - 4
  xmin[xmin == 0] <- -1e-5
  return(xmin)
}
