Schaffer7BBOB <- function(x) {
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
  

  
  xopt <<- get_xmin(length(x))
  dim <<- length(x)
  fopt <<- get_fmin(dim)
  funid <<- 17
  rotation <<- compute_rotation(17 + 1e+6, dim)
  linearTF <<- compute_rotation(17, dim) %*% diag(sqrt(10) ^ (seq(0, 1, length.out = dim)))
  arrexpo <<- 0.5 * seq(0, 1, length.out = dim)
  
  z <- x_shift(x, xopt) %*% rotation
  idx <- z > 0
  z[idx] <- z[idx] ^ (1 + arrexpo[idx] * sqrt(z[idx]))
  z <- z %*% linearTF
  s <- z[-length(z)]^2 + z[-1]^2
  
  y <- mean((s^0.25) * (sin(50 * s^0.1)^2 + 1))^2 + 10 * pen(x) + fopt
  
  return(y)
}

get_xl <- function(nx) {
  return(rep(-5, nx))
}

get_xu <- function(nx) {
  return(rep(5, nx))
}

pen <- function(x) {
  return(sum((pmax(0, abs(x) - 5))^2))
}

x_shift <- function(x, xopt) {
  return(t(x - xopt))
}

compute_rotation <- function(seed, dim) {
  B <- matrix(gauss(dim * dim, seed), nrow = dim, byrow = TRUE)
  for (i in 1:dim) {
    for (j in 1:(i - 1)) {
      B[i, ] <- B[i, ] - sum(B[i, ] * B[j, ]) * B[j, ]
    }
    B[i, ] <- B[i, ] / sqrt(sum(B[i, ]^2))
  }
  return(B)
}

gauss <- function(N, seed) {
  r <- unif(2 * N, seed)
  g <- sqrt(-2 * log(r[1:N])) * cos(2 * pi * r[(N + 1):(2 * N)])
  g[g == 0] <- 1e-99
  return(g)
}

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
  
  for (i in 1:N) {
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

get_fmin <- function(nx) {
  return(min(1000, max(-1000, round(100 * 100 * gauss(1, 17) / gauss(1, 17 + 1)) / 100)))
}

get_xmin <- function(nx) {
  xmin <- 8 * floor(1e+4 * unif(nx, 17)) / 1e+4 - 4
  xmin[xmin == 0] <- -1e-5
  return(xmin)
}
