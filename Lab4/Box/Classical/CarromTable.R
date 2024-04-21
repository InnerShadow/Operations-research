CarromTable <- function(x) {
  # -------------------------------------------------------------------------
  # R coding by: [ВАШЕ ИМЯ]
  # Name:
  #   CarromTable.R
  #
  # References:                                    
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
  #   f = -24.15681554739123981790
  #   x = c(9.64616767015461995527, -9.64616767015468212776)
  #
  # Default variable bounds:
  #   -10 <= x[i] <= 10, i = 1,...,n
  #
  # Problem Properties:
  #   n  = 2;
  #   #g = 0;
  #   #h = 0;
  #
  # Known characteristics of test function:
  #   Differentiable, Non-separable, Non-scalable, Multi-modal,
  #   Non-convex, Non-plateau, Non-Zero-Solution, Symmetric
  # -------------------------------------------------------------------------
  
  if (missing(x)) {
    y <- list(
      nx = 2,
      ng = 0,
      nh = 0,
      xl = function(nx) get_xl(nx),
      xu = function(nx) get_xu(nx),
      fmin = function(nx) get_fmin(),
      xmin = function(nx) get_xmin(),
      features = c(1, 0, 0, 1, 0, 0, 0, 1),
      libraries = c(0, 0, 0, 1, 1, 0, 1, 0, 0, 0)
    )
    return(y)
  }
  

  
  y <- -((cos(x[1]) * cos(x[2]) * exp(abs(1 - sqrt(x[1]^2 + x[2]^2) / pi)))^2) / 30
  
  return(y)
}

get_xl <- function(nx) {
  xl <- rep(-10, nx)
  return(xl)
}

get_xu <- function(nx) {
  xu <- rep(10, nx)
  return(xu)
}

get_fmin <- function() {
  fmin <- -24.15681554739123981790
  return(fmin)
}

get_xmin <- function() {
  xmin <- c(9.64616767015461995527, -9.64616767015468212776)
  return(xmin)
}
