SharpRidgeBBOB <- function(x) {
  if (missing(x)) {
    nx <- 0
    ng <- 0
    nh <- 0
    xl <- function(nx) get_xl(nx)
    xu <- function(nx) get_xu(nx)
    fmin <- function(nx) get_fmin(nx)
    xmin <- function(nx) get_xmin(nx)
    features <- c(0, 0, 1, 0, 1, 0, 0, 0)
    libraries <- c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
    return(list(nx = nx, ng = ng, nh = nh, xl = xl, xu = xu, fmin = fmin, xmin = xmin, features = features, libraries = libraries))
  }
  

  
  if (!exists("xopt") || !exists("dim") || !exists("fopt") || !exists("funid") || !exists("linearTF") || is.null(xopt) || is.null(dim) || is.null(fopt) || is.null(funid) || is.null(linearTF) || dim != length(x) || funid != 13) {
    dim <- length(x)
    xopt <- get_xmin(dim)
    fopt <- get_fmin(dim)
    funid <- 13
    linearTF <- (compute_rotation(13, dim) %*% diag(sqrt(10) ^ seq(0, 1, length.out = dim))) %*% compute_rotation(13 + 1e+6, dim)
  }
  
  z <- t(x_shift(t(x), xopt)) %*% linearTF
  y <- z[1]^2 + 100 * sqrt(sum(z[-1]^2)) + fopt
  return(y)
}

get_xl <- function(nx) {
  return(rep(-5, nx))
}

get_xu <- function(nx) {
  return(rep(5, nx))
}

x_shift <- function(x, xopt) {
  return(x - xopt)
}

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
  fmin <- min(c(1000, max(c(-1000, (round(100 * 100 * gauss(1, 13) / gauss(1, 13 + 1)) / 100)))))
  return(fmin)
}

get_xmin <- function(nx) {
  xmin <- 8 * floor(1e+4 * unif(nx, 13)) / 1e+4 - 4
  xmin[xmin == 0] <- -1e-5
  return(xmin)
}
