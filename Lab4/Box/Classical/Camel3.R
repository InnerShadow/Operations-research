Camel3 <- function(x) {
  # -------------------------------------------------------------------------
  # R coding by: [ВАШЕ ИМЯ]
  # Name:
  #   Camel3.R
  #
  # References:                                      
  #  - Surjanovic, S., Bingham, D. (2013): Virtual library of simulation 
  #    experiments: Test functions and datasets. 
  #    URL: http://www.sfu.ca/~ssurjano/index.html  
  #  - Gavana, A.: Global optimization benchmarks and ampgo. 
  #    URL: http://infinity77.net/global_optimization/index.html 
  #  - Momin Jamil and Xin-She Yang, A literature survey of benchmark functions for
  #    global optimization problems, Int. Journal of Mathematical Modelling and
  #    Numerical Optimisation, Vol. 4, No. 2, pp. 150–194 (2013).
  #    DOI: 10.1504/IJMMNO.2013.055204
  #  - Rody Oldenhuis (2020): Test functions for global optimization algorithms
  #    URL: https://github.com/rodyo/FEX-testfunctions/releases/tag/v1.5
  #
  # Globally optimal solution:
  #   f = 0
  #   x = c(0, 0)
  #
  # Default variable bounds:
  #   -5 <= x[i] <= 5, i = 1,...,n
  #
  # Problem Properties:
  #   n  = 2;
  #   #g = 0;
  #   #h = 0;
  #
  # Known characteristics of test function:
  #   Differentiable, Non-separable, Non-scalable, Multi-modal,
  #   Non-convex, Non-plateau, Zero-Solution, Asymmetric
  # -------------------------------------------------------------------------
  
  if (missing(x)) {
    y <- list(
      nx = 2,
      ng = 0,
      nh = 0,
      xl = function(nx) get_xl(nx),
      xu = function(nx) get_xu(nx),
      fmin = function(nx) get_fmin(nx),
      xmin = function(nx) get_xmin(nx),
      features = c(1, 0, 0, 1, 0, 0, 1, 0),
      libraries = c(0, 1, 0, 1, 1, 0, 1, 0, 0, 0)
    )
    return(y)
  }
  

  
  x1 <- x[1]
  x2 <- x[2]
  
  term1 <- 2 * x1^2
  term2 <- -1.05 * x1^4
  term3 <- x1^6 / 6
  term4 <- x1 * x2
  term5 <- x2^2
  
  y <- term1 + term2 + term3 + term4 + term5
  
  return(y)
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
  fmin <- 0
  return(fmin)
}

get_xmin <- function(nx) {
  xmin <- c(0, 0)
  return(xmin)
}
