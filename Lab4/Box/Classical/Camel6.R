Camel6 <- function(x) {
  # -------------------------------------------------------------------------
  # R coding by: [ВАШЕ ИМЯ]
  # Name:
  #   Camel6.R
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
  #   f = -1.03162845348987741723
  #   x = c(-0.08984201372191424895, 0.71265640200326663134)
  #
  # Default variable bounds:
  #   -3 <= x[1] <= 3
  #   -2 <= x[2] <= 2
  #
  # Problem Properties:
  #   n  = 2;
  #   #g = 0;
  #   #h = 0;
  #
  # Known characteristics of test function:
  #   Differentiable, Non-separable, Non-scalable, Multi-modal,
  #   Non-convex, Non-plateau, Non-Zero-Solution, Asymmetric
  # -------------------------------------------------------------------------
  
  if (missing(x)) {
    y <- list(
      nx = 2,
      ng = 0,
      nh = 0,
      xl = function(nx) get_xl(),
      xu = function(nx) get_xu(),
      fmin = function(nx) get_fmin(),
      xmin = function(nx) get_xmin(),
      features = c(1, 0, 0, 1, 0, 0, 0, 0),
      libraries = c(0, 1, 0, 1, 1, 0, 1, 0, 0, 0)
    )
    return(y)
  }
  

  
  x1 <- x[1]
  x2 <- x[2]
  
  term1 <- (4 - 2.1 * x1^2 + (x1^4) / 3) * x1^2
  term2 <- x1 * x2
  term3 <- (-4 + 4 * x2^2) * x2^2
  
  y <- term1 + term2 + term3
  
  return(y)
}

get_xl <- function() {
  xl <- c(-3, -2)
  return(xl)
}

get_xu <- function() {
  xu <- c(3, 2)
  return(xu)
}

get_fmin <- function() {
  fmin <- -1.03162845348987741723
  return(fmin)
}

get_xmin <- function() {
  xmin <- c(-0.08984201372191424895, 0.71265640200326663134)
  return(xmin)
}
