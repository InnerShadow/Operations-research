# -------------------------------------------------------------------------
# R coding by: [Your Name]
# Name:
#   AbstractEllipsoidalBBOB.R
#
# References:
#  - Nikolaus Hansen, Anne Auger, Steffen Finck, Raymond Ros. Real-Parameter 
#    Black-Box Optimization Benchmarking 2010: Experimental Setup. 
#    [Research Report] RR-7215, INRIA. 2010. ffinria-00462481
#
# Globally optimal solution:
#   f = -78.99
#   x = Depends on dimension
#
# Default variable bounds:
#   -5 <= x[i] <= 5, i = 1,...,n
#   
# Problem Properties:
#   n  = any dimension;
#   #g = 0;
#   #h = 0;
#
# Known characteristics of test function:
#   Differentiable, Non-separable, Scalable, Uni-modal,
#   Non-Convex, Non-plateau, Non-Zero-Solution, Asymmetric
# -------------------------------------------------------------------------

AbstractEllipsoidalBBOB <- function(x) {
  if (missing(x)) {
    y <- list(
      nx = 0,
      ng = 0,
      nh = 0,
      xl = function(nx) get_xl(nx),
      xu = function(nx) get_xu(nx),
      fmin = function(nx) get_fmin(nx),
      xmin = function(nx) get_xmin(nx),
      features = c(1, 0, 1, 0, 0, 0, 0, 0),
      libraries = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
    )
    return(y)
  }
  

  
  # Define persistent variables
  dim <- length(x)
  xopt <- get_xmin(dim)
  fopt <- get_fmin(dim)
  funid <- 10
  scales <- (1e+6) ^ seq(0, 1, length.out = dim)
  R <- compute_rotation(10 + 1e+6, dim)
  
  z <- toz(x_shift(t(x), xopt) %*% R)
  y <- sum(scales * (z^2)) + fopt
  return(y)
}

x_shift <- function(x, xopt) {
  z <- x - xopt
  return(z)
}

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

compute_rotation <- function(seed, dim) {
  B <- t(matrix(gauss(dim * dim, seed), nrow = dim))
  for (i in seq_len(dim)) {
    for (j in seq_len(i - 1)) {
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

get_xl <- function(nx) {
  xl <- rep(-5, nx)
  return(xl)
}

get_xu <- function(nx) {
  xu <- rep(5, nx)
  return(xu)
}

get_fmin <- function(nx) {
  fmin <- min(1000, max(-1000, (round(100 * 100 * gauss(1, 10) / gauss(1, 10 + 1)) / 100)))
  return(fmin)
}

get_xmin <- function(nx) {
  xmin <- 8 * floor(1e+4 * unif(nx, 10)) / 1e+4 - 4
  xmin[xmin == 0] <- -1e-5
  return(xmin)
}
