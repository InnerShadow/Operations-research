RastriginBBOB <- function(x) {
  # Function constants
  scale <- sqrt(10)^(seq(0, 1, length.out = length(x)))
  dim <- length(x)
  xopt <- get_xmin(dim)
  fopt <- get_fmin(dim)
  funid <- 3
  
  # Reshape x if necessary

  
  # Define helper functions
  get_xl <- function(nx) {
    return(rep(-5, nx))
  }
  
  get_xu <- function(nx) {
    return(rep(5, nx))
  }
  
  asy <- function(x, beta) {
    idx <- (x > 0)
    arrexpo <- beta * seq(0, 1, length.out = length(x))
    x[idx] <- x[idx]^(1 + arrexpo[idx] * sqrt(x[idx]))
    return(x)
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
    fmin <- min(c(1000, max(c(-1000, round(100 * 100 * gauss(1, 3) / gauss(1, 3 + 1)) / 100))))
    return(fmin)
  }
  
  get_xmin <- function(nx) {
    xmin <- 8 * floor(1e+4 * unif(nx, 3)) / 1e+4 - 4
    xmin[xmin == 0] <- -1e-5
    return(xmin)
  }
  
  # Main function
  z <- asy(toz(x_shift(x, xopt)), 0.2) * scale
  y <- 10 * (dim - sum(cos(2 * pi * z))) + sum(z^2) + fopt
  
  return(y)
}

# Test the function
# Define sample input x
x <- c(1, 2, 3)

# Call the function
result <- RastriginBBOB(x)
print(result)

