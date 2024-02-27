library(plotly)

# Постройте график функции и её производных $F=(x-5)^2+6x$. Найдите минимум функции.

F <- function(x){
  (x - 5) ^ 2 + 6 * x
}

dF <- function(x){
  2 * (x - 5) + 6
}

x <- seq(-5, 10, by = 0.1)
y <- F(x)
dy <- dF(x)
data <- data.frame(x, y)
Ddata <- data.frame(x, dy)
fig <- plot_ly(data, x = ~x, y = ~y,type = 'scatter', mode = 'lines') %>%
  add_trace(Ddata, x = ~x, y = ~dy,type = 'scatter', mode = 'lines')

fig

op <- optimise(F, c(-5, 10))
cat(op$objective)
cat(F(op$minimum))
