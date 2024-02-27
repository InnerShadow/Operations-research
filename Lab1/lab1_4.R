library(plotly)

# Найдите частные производные функции $F=xy$ и точку, где они равны 0. 
# Постройте объемный и контурный график. Какие особые точки вы можете выделить.

F <- function(x, y){
  x * y
}

x <- y <- seq(-5, 5, length.out = 100)
z <- outer(x, y, FUN = F)
plot_ly(z = ~z) |> add_surface()

z <- outer(x, y, FUN = F)
plot_ly(x = ~x, y = ~y, z = ~z, type = "contour")

solution <- optim(c(0, 0), function(vec) F(vec[1], vec[2]))
min_point <- solution$par
min_point
F(min_point[1], min_point[2])
