# Function for WeierstrassBBOB
WeierstrassBBOB <- function(x) {
  if (missing(x)) {
    y <- list()
    y$nx <- 0
    y$ng <- 0
    y$nh <- 0
    y$xl <- function(nx) get_xl(nx)
    y$xu <- function(nx) get_xu(nx)
    y$fmin <- function(nx) get_fmin(nx)
    y$xmin <- function(nx) get_xmin(nx)
    y$features <- c(1, 0, 1, 1, 0, 0, 0, 0)
    y$libraries <- c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
    return(y)
  }
  

  
  xopt <<- get_xmin(length(x))
  dim <<- length(x)
  fopt <<- get_fmin(length(x))
  funid <<- 16
  rotation <<- compute_rotation(16 + 1e+6, length(x))
  linearTF <<- (compute_rotation(16, length(x)) * diag(sqrt(1/100)^seq(0, 1, length.out = length(x)))) %*% rotation
  
  z <- toz(x_shift(x, xopt)) %*% linearTF
  sum1 <- sum((0.5^(0:11)) * cos(pi * (3^(0:11))))
  sum2 <- function(ii) sum((0.5^(0:11)) * cos((z[ii] + 0.5) * 2 * pi * (3^(0:11))))
  y <- 0
  for (i in 1:dim) {
    y <- y + sum2(i)
  }
  y <- 10 * ((y / dim) - sum1)^3 + fopt + (10 / dim) * pen(x)
  return(y)
}

get_xl <- function(nx) {
  return(rep(-5, nx))
}

get_xu <- function(nx) {
  return(rep(5, nx))
}

pen <- function(x) {
  return(sum((pmax(0, abs(x) - 5))^2 * sign(x)))
}

x_shift <- function(x, xopt) {
  return(t(x - xopt))
}

toz <- function(x) {
  z <- x
  for (i in 1:length(x)) {
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
  for (i in 1:dim) {
    for (j in 1:(i-1)) {
      B[i,] <- B[i,] - sum(B[i,] * B[j,]) * B[j,]
    }
    B[i,] <- B[i,] / sqrt(sum(B[i,]^2))
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
  fmin <- min(c(1000, max(-1000, (round(100 * 100 * gauss(1, 16) / gauss(1, 16 + 1)) / 100))))
  return(fmin)
}

get_xmin <- function(nx) {
  xmin <- 8 * floor(1e+4 * unif(nx, 16)) / 1e+4 - 4
  xmin[xmin == 0] <- -1e-5
  return(xmin)
}

