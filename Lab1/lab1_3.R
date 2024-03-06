library(plotly) 

F <- function(x, y){
  3 * x^2 + x * y + 2 * y ^ 2 - x - 4 * y
}

x <- y <- seq(-5, 5, length.out = 100)
z <-t(outer(x, y, FUN = F))
plot_ly(z = ~z) |> add_surface()

z <- t(outer(x, y, FUN = F))
plot_ly(x = ~x, y = ~y, z = ~z, type = "contour")

library(plotly) 

solution <- optim(c(0, 0), function(vec) F(vec[1], vec[2]))
min_point <- solution$par
min_point
cat(F(min_point[1], min_point[2]))

