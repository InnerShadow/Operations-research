library(plotly) 

F <- function(x, r = 1){
  cos_value <- x / sqrt(x^2 + r^2)
  cos_value / (x^2 + r^2)
}

dF <- function(x, r = 1){
  (r^2 -2 * x^2) / (r^2 + x^2) ^ (5 / 2)
}

r <- c(1)
x <- seq(0, 5, length.out = 5000)
y <- F(x, r)
dy <- dF(x, r)

data <- data.frame(x, y)
dData <- data.frame(x, dy)

fig <- plot_ly(data, x = ~x, y = ~y,type = 'scatter', mode = 'lines') |> 
  add_trace(Ddata, x = ~x, y = ~dy, type = 'scatter', mode = 'lines')

fig

op <- optimise(F, c(0, 5), maximum = T)
cat(op$maximum, "\n")
cat(F(op$maximum))

