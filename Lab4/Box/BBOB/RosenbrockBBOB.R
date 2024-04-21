RosenbrockBBOB <- function(x) {
  # Parameters
  fopt <- -135.13
  dim <- length(x)
  xopt <- get_xmin(dim)
  funid <- 8
  
  # Check if input is vector
  if (is.matrix(x) && nrow(x) > ncol(x)) {
    x <- t(x)
  }
  
  # Objective function
  z <- max(c(1, sqrt(dim) / 8)) * x_shift(x, xopt) + 1
  y <- sum(100 * (z[1:(length(z) - 1)]^2 - z[2:length(z)])^2) + 
    sum((z[1:(length(z) - 1)] - 1)^2) + fopt + pen(x)
  
  return(y)
}

get_xl <- function(nx) {
  return(rep(-5, nx))
}

get_xu <- function(nx) {
  return(rep(5, nx))
}

pen <- function(x) {
  return(sum((pmax(0, abs(x) - 5) * sign(x))^2))
}

x_shift <- function(x, xopt) {
  return(x - xopt)
}

gauss <- function(N, seed) {
  r <- unif(2 * N, seed)
  g <- sqrt(-2 * log(r[1:N])) * cos(2 * pi * r[(N + 1):(2 * N)])
  g[g == 0] <- 1e-99
  return(g)
}

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

get_fmin <- function(_) {
  fmin <- min(1000, max(-1000, (round(100 * 100 * gauss(1, 8) / gauss(1, 8 + 1)) / 100)))
  return(fmin)
}

get_xmin <- function(nx) {
  xmin <- 8 * floor(1e+4 * unif(nx, 8)) / 1e+4 - 4
  xmin[xmin == 0] <- -1e-5
  xmin <- 0.75 * xmin
  return(xmin)
}

