LinearSlopeBBOB <- function(x) {
  # ------------------------------------------------------------------------- 
  # MATLAB coding by: Linas Stripinis 
  # Name: 
  #   LinearSlopeBBOB.m 
  # 
  # References: 
  #  - Nikolaus Hansen, Anne Auger, Steffen Finck, Raymond Ros. Real-Parameter  
  #    Black-Box Optimization Benchmarking 2010: Experimental Setup.  
  #    [Research Report] RR-7215, INRIA. 2010. ffinria-00462481 
  # 
  # Globally optimal solution: 
  #   f = 51.53 
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
  #   Differentiable, Separable, Scalable, Uni-modal, 
  #   Convex, Non-plateau, Non-Zero-Solution, Asymmetric 
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
      features = c(1, 1, 1, 0, 1, 0, 0, 0),
      libraries = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
    )
    return(y)
  }
  

  
  # Initialization
  xopt <- get_xmin(length(x))
  dim <- length(x)
  fopt <- get_fmin(dim)
  funid <- 5
  scale <- (-sign(xopt) * (sqrt(100) ^ seq(0, 1, length.out = dim)))
  
  idx_out_of_bounds <- x * xopt > 25
  x[idx_out_of_bounds] <- sign(x[idx_out_of_bounds]) * 5
  
  y <- fopt + 5 * sum(abs(scale)) + sum(x * scale)
  
  return(y)
}

get_xl <- function(nx) {
  return(rep(-5, nx))
}

get_xu <- function(nx) {
  return(rep(5, nx))
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
  
  rgrand <- rep(0, 32)
  aktseed <- inseed
  
  for (i in 39:0) {
    tmp <- floor(aktseed / 127773)
    aktseed <- 16807 * (aktseed - tmp * 127773) - 2836 * tmp
    if (aktseed < 0) aktseed <- aktseed + 2147483647
    if (i < 32) rgrand[i + 1] <- aktseed
  }
  
  aktrand <- rgrand[1]
  r <- rep(0, N)
  
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
  gauss_vals <- gauss(1, 5)
  fmin <- min(c(1000, max(c(-1000, (round(100 * 100 * gauss_vals[1] / gauss(1, 5 + 1)) / 100)))))
  return(fmin)
}

get_xmin <- function(nx) {
  xmin <- 8 * floor(1e+4 * unif(nx, 5)) / 1e+4 - 4
  xmin <- 5 * sign(xmin)
  return(xmin)
}


