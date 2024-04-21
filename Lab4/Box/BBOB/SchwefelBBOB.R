SchwefelBBOB <- function(x) {
  # Function definition
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
  

  
  # Variable Initialization
  dim <- length(x)
  xopt <- get_xmin(dim)
  fopt <- get_fmin(dim)
  funid <- 20
  scales <- sqrt(10) ^ seq(0, 1, length.out = dim)
  
  # Compute the function value
  arrxopt <- 2 * abs(xopt)
  arrsigns <- sign(xopt)
  z <- 2 * arrsigns * x
  z[-1] <- z[-1] + 0.25 * (z[-length(z)] - arrxopt[-length(arrxopt)])
  z <- 100 * (scales * (z - arrxopt) + arrxopt)
  y <- 0.01 * (418.9828872724339 - mean(z * sin(sqrt(abs(z))))) + pen(z) + fopt
  
  return(y)
}

get_xl <- function(nx) {
  return(rep(-5, nx))
}

get_xu <- function(nx) {
  return(rep(5, nx))
}

pen <- function(x) {
  return(0.01 * sum((pmax(0, abs(x) - 500) * sign(x))^2))
}

get_fmin <- function(nx) {
  return(min(1000, max(-1000, round(100 * 100 * gauss(1, 20) / gauss(1, 20 + 1)) / 100)))
}

get_xmin <- function(nx) {
  return(0.5 * sign(unif(nx, 20) - 0.5) * 4.2096874633)
}

gauss <- function(N, seed) {
  r <- unif(2 * N, seed)
  g <- sqrt(-2 * log(r[seq_len(N)])) * cos(2 * pi * r[(N + 1):(2 * N)])
  g[g == 0] <- 1e-99
  return(g)
}

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
  for (i in seq_len(N)) {
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
