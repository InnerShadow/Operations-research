ABS_6 <- function(x = NULL) {
  # Проверяем, задан ли аргумент x
  if (is.null(x)) {
    return(list(nx = 0, ng = 0, nh = 0, xl = get_xl, xu = get_xu, 
                fmin = get_fmin, xmin = get_xmin, features = c(0, 1, 1, 1, 0, 0, 1, 1), 
                libraries = c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0)))
  }
  

  
  m <- 0.9000
  k <- 8
  lmbd <- 0.9000
  
  f <- function(x) {
    return(zigzag(x, k, m, lmbd) * 3 * (abs(log(abs(x) * 1000 + 1))) + 30 - 30 * abs(cos(x / (pi * 10))))
  }
  
  y <- sum(apply(x, 2, f))
  return(y)
}

zigzag <- function(xs, k, m, lmbd) {
  xs <- abs(xs)
  xs <- xs / k - floor(xs / k)
  ids <- xs <= lmbd
  valz <- 1 - m + ifelse(ids, m * (xs / lmbd), m * (1 - (xs - lmbd) / (1 - lmbd)))
  return(valz)
}

get_xl <- function(nx) {
  return(rep(-100, nx))
}

get_xu <- function(nx) {
  return(rep(100, nx))
}

get_fmin <- function(nx) {
  return(0)
}

get_xmin <- function(nx) {
  return(rep(0, nx))
}


