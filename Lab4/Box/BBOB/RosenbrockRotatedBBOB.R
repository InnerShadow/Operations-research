RosenbrockRotatedBBOB <- function(x) {
  # Check if input is empty
  if (missing(x)) {
    y <- list(nx = 0,
              ng = 0,
              nh = 0,
              xl = function(nx) get_xl(nx),
              xu = function(nx) get_xu(nx),
              fmin = function(nx) get_fmin(nx),
              xmin = function(nx) get_xmin(nx),
              features = c(1, 0, 1, 1, 0, 0, 0, 0),
              libraries = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0))
    return(y)
  }
  
  # Transpose x if necessary

  
  # Define persistent variables
  if (exists("xopt") && exists("dim") && exists("fopt") && exists("funid") && exists("linearTF")) {
    if (is.null(dim) || dim != length(x) || funid != 9) {
      dim <<- length(x)
      xopt <<- get_xmin(dim)
      fopt <<- get_fmin(dim)
      funid <<- 9
      linearTF <<- max(c(1, sqrt(dim)/8)) * compute_rotation(9, dim)
    }
  } else {
    dim <<- length(x)
    xopt <<- get_xmin(dim)
    fopt <<- get_fmin(dim)
    funid <<- 9
    linearTF <<- max(c(1, sqrt(dim)/8)) * compute_rotation(9, dim)
  }
  
  z <- t(x) %*% linearTF + 0.5
  y <- sum(100 * (z[1:(length(z) - 1)]^2 - z[2:length(z)])^2) +
    sum((z[1:(length(z) - 1)] - 1)^2) + fopt
  
  return(y)
}

get_xl <- function(nx) {
  return(rep(-5, nx))
}

get_xu <- function(nx) {
  return(rep(5, nx))
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

get_fmin <- function(nx) {
  fmin <- min(1000, max(-1000, (round(100 * 100 * gauss(1, 9) / gauss(1, 9 + 1)) / 100)))
  return(fmin)
}

get_xmin <- function(nx) {
  scale <- max(1, sqrt(nx) / 8)
  linearTF <- scale * compute_rotation(9, nx)
  xmin <- (0.5 * rep(1, nx)) %*% solve(linearTF)
  return(xmin)
}

