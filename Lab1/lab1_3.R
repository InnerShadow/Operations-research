library(plotly)

# Постройте объемный и контурный график функции $F=3x^2+xy+2y^2-x-4y$. 
# Найдите частные производные. Определите точки максимума, минимума и перегиба.

F <- function(x, y){
  3 * x^2 + x * y + 2 * y ^ 2 - x - 4 * y
}

dFdx <- function(x, y){
  6 * x + y - 1 
}

dFdy <- function(x, y){
  x + 4 * y - 4
}

x <- y <- seq(-5, 5, length.out = 100)
z <- outer(x, y, FUN = F)
plot_ly(z = ~z) |> add_surface()

z <- outer(x, y, FUN = F)
plot_ly(x = ~x, y = ~y, z = ~z, type = "contour")

solution <- optim(c(0, 0), function(vec) F(vec[1], vec[2]))
min_point <- solution$par
min_point
cat(F(min_point[1], min_point[2]))

