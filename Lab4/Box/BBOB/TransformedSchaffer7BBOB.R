TransformedSchaffer7BBOB <- function(x) {
  # -------------------------------------------------------------------------
  # MATLAB coding by: Linas Stripinis
  # Name:
  #   TransformedSchaffer7BBOB.m
  #
  # References:
  #  - Nikolaus Hansen, Anne Auger, Steffen Finck, Raymond Ros. Real-Parameter 
  #    Black-Box Optimization Benchmarking 2010: Experimental Setup. 
  #    [Research Report] RR-7215, INRIA. 2010. ffinria-00462481
  #
  # Globally optimal solution:
  #   f = -208.28
  #   x = Depends on dimension
  #
  # Default variable bounds:
  #   -5 <= x(i) <= 5, i = 1,...,n
  #   
  # Problem Properties:
  #   n  = any dimension;
  #   #g = 0;
  #   #h = 0;
  #
  # Known characteristics of test function:
  #   Differentiable, Non-separable, Scalable, Multi-modal,
  #   Non-convex, Non-plateau, Non-Zero-Solution, Asymmetric
  # -------------------------------------------------------------------------
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
  fopt <<- get_fmin(length(x))
  funid <<- 18
  rotation <<- compute_rotation(18 + 1e+6, dim)
  linearTF <<- compute_rotation(18, dim) %*% diag(sqrt(1000) ^ seq(0, 1, length.out = dim))
  arrexpo <<- 0.5 * seq(0, 1, length.out = dim)
  
  z <- x_shift(x, xopt) %*% t(rotation)
  idx <- z > 0
  z[idx] <- z[idx] ^ (1 + arrexpo[idx] * sqrt(z[idx]))
  z <- z %*% linearTF
  s <- z[1:(length(z) - 1)] ^ 2 + z[2:length(z)] ^ 2
  y <- (mean((s ^ 0.25) * (sin(50 * s ^ 0.1) ^ 2 + 1))) ^ 2 + 10 * pen(x) + fopt
  return(y)
}

get_xl <- function(nx) {
  return(rep(-5, nx))
}

get_xu <- function(nx) {
  return(rep(5, nx))
}

pen <- function(x) {
  z <- sum((pmax(rep(0, length(x)), (abs(x) - 5))) ^ 2)
  return(z)
}

x_shift <- function(x, xopt) {
  return(t(x - xopt))
}

compute_rotation <- function(seed, dim) {
  B <- matrix(gauss(dim * dim, seed), nrow = dim, ncol = dim)
  for (i in 1:dim) {
    for (j in 1:(i - 1)) {
      B[i,] <- B[i,] - sum(B[i,] * B[j,]) * B[j,]
    }
    B[i,] <- B[i,] / sqrt(sum(B[i,] ^ 2))
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

get_fmin <- function(nx) {
  fmin <- min(c(1000, max(c(-1000, (round(100 * 100 * gauss(1, 18) / gauss(1, 18 + 1)) / 100)))))
  return(fmin)
}

get_xmin <- function(nx) {
  xmin <- 8 * floor(1e+4 * unif(nx, 18)) / 1e+4 - 4
  xmin[xmin == 0] <- -1e-5
  return(xmin)
}
