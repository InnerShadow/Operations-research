# Определение функции ABS_4
ABS_4 <- function(x) {
  if (missing(x)) {
    return(list(nx = 0, ng = 0, nh = 0,
                xl = function(nx) return(rep(-100, nx)),
                xu = function(nx) return(rep(100, nx)),
                fmin = function(nx) return(0),
                xmin = function(nx) return(rep(0, nx)),
                features = c(0, 1, 1, 1, 0, 0, 1, 1),
                libraries = c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0)))
  }
  

  
  m <- 1
  k <- 1
  lmbd <- 0.1
  
  # Вложенная функция zigzag
  zigzag <- function(xs, k, m, lmbd) {
    xs <- abs(xs)
    xs <- xs / k - floor(xs / k)
    ids <- xs <= lmbd
    return(1 - m + ids * m * (xs / lmbd) + (!ids) * m * (1 - (xs - lmbd) / (1 - lmbd)))
  }
  
  # Основная функция
  f <- function(x) {
    10 * abs(sin(0.1 * x)) + 3e-9 * zigzag(x, k, m, lmbd) * abs((x - 40) * (x - 185) * x * (x + 50) * (x + 180))
  }
  
  return(sum(f(x)))
}

# Вспомогательные функции
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


