---
title: Лабораторная работа 3. Задания
author: "Сікаленка Міхаіл Аляксандравіч"
date: "01/04/2024"
output:
  html_document:
    df_print: paged
    keep_md: true
  word_document: default
  pdf_document:
    latex_engine: lualatex
    includes:
      in_header: preamble.tex
editor_options: 
  markdown: 
    wrap: sentence
---

# Задача

Рассмотрим телекоммуникационную сеть, состоящую из $p$ узлов-маршрутизаторов $R$ и $q$ соединяющих их линий связи $P$.
Каждая линия связи оценивается временем задержки сигнала измеряемым в миллисекундах: $$W=\{w_1,w_2,...,w_q\}$$ Маршрутизаторы, имеют горячее резервирование кратности: $$SR=\{sr_1,sr_2,...,sr_p\}\\ 1\leq sr_i\leq 5$$ Линии связи имеют дублирование кратности: $$SP=\{sp_1,sp_2,...,sp_q\}\\ 1\leq sp_i\leq 5$$ Для высококритичных приложений, например, для военной техники, обязателен резерв канала боевого управления, а скорость передачи данных оказывает существенное влияние на отклик системы в целом.
Требуется найти два (в общем случае можно рассматривать $r$) зарезервированных канала от узла $k$ к узлу $l$, не задействующих совместно незадублированные маршрутизаторы и линии связи, обеспечивающих суммарно минимальное время задержки.

# Указания к решению.

1.  Найти все $N$ не образующие петель маршруты $Rt$ от узла $k$ к узлу $l$.
    Это можно сделать вручную.
    На высокую оценку необходимо разработать и реализовать алгоритм, осуществляющий построение таких маршрутов.
    Допускается использование библиотечной функции.

2.  Занумеруем все возможные маршруты от $1$ до $n$ и сопоставим каждому из них соответствующее время задержки $t_i$, равное сумме времен по всем ребрам маршрута $i$.

3.  Введем в качестве варьируемых параметров бинарные переменные $x_i$, $i=1,...,n$.
    Здесь $x_i=1$, если маршрут номер $i$ выбран и $x_i=0$, если нет.
    Тогда целевая функция, которую необходимо минимизировать, будет иметь вид: $$F(\vec{x})=\sum_{i=1}^{N}t_ix_i\\$$ Так как необходимо найти $r$ независимых каналов, первое ограничение имеет вид (в простейшем случае для двукратного резервирования $r=2$): $$\sum_{i=1}^{N}x_i=r$$

4.  Далее требуется построить ограничение ресурсов для маршрутов.
    Столбцами матрыцы ограничений будут маршруты, а по строкам задействованные маршрутизаторы и линии связи.
    Следовательно, для всех маршрутов использующих совместно маршрутизатор (линию связи) можно записать ограничение: $$\sum_{i\in V(Rt)} x_i \leq SR\\ \sum_{i\in E(Rt)} x_i \leq SP$$

    В теории графов для графа $g$ множество его вершин обозначается как $V(g)$, а множество его рёбер $E(g)$.

5.  Решаем задачу линейного целочисленного программирования.

6.  Построить граф с выделенными найденными маршрутами.

# Варианты

Введите ваше ФИО в параметр функции **digest2int**


```r
# Import list & set seed
# install.packages('extraDistr')
library(digest)
library(extraDistr)
library(lpSolve)
library(igraph)
library(stringr)

set.seed(digest2int('Мозоль Назар Русланович'))
```

Переменная *simple* отвечает за выбор варианта сложности.


```r
# Do difficult options
simple <- T
```

Далее на основании вашего ФИО и варианта сложности генерируются основные параметры задачи.
Число маршрутизаторов $p$:


```r
# Get number of routers
p <- ifelse(simple, rdunif(1, 6, 10), rdunif(1, 20, 50))
p
```

```
## [1] 9
```

Число резервных маршрутов $r$


```r
# Get number of backup routes
r <- ifelse(simple,2,rdunif(1,4,5))
r
```

```
## [1] 2
```

Граф маршрутизации


```r
# generate graph 
repeat {
g <<- sample_gnp(p, ifelse(simple,0.2,0.05))
if(is_connected(g))
  break
}
```

Количество линий связи $q$


```r
# Get number of communication lines
q <- length(E(g))
q
```

```
## [1] 9
```

Задержка линии связи $W$


```r
# Get communication line delay
E(g)$weight <- rdunif(q, 1, 100)
```

Резервирование маршрутизаторов $SR$


```r
# Get Router redundancy
V(g)$standby <- if(simple) rep(2,p) else rdunif(p, r, 5)
```

Резервирование линий связи $SP$


```r
# Get reservation of communication lines
E(g)$standby <- if(simple) rep(2,q) else rdunif(q, r, 5)
```

Узлы, между которыми требуется обеспечить надежную связь


```r
# Get vertexes that we need to connect
v <- farthest_vertices(g)$vertices
v
```

```
## + 2/9 vertices, from 078f161:
## [1] 6 9
```

```r
k <- v[1]
l <- v[2]
```

# Ваш вариант



Общая формулировка задачи выглядит следующим образом **(подстановка реализуется при сохранении, например html)**:

Рассмотрим телекоммуникационную сеть, состоящую из $p=9$ узлов-маршрутизаторов $R$ и $q=9$ соединяющих их линий связи $P$.
Сеть определяется граформ, заданным списком смежности: $$
\newcommand\ue{\mathrel{\bullet\mkern-3mu{-}\mkern-3mu\bullet}}
\{1\ue3,2\ue5,4\ue7,5\ue7,1\ue8,2\ue8,3\ue8,6\ue8,5\ue9\}
$$ Каждая линия связи оценивается временем задержки сигнала измеряемым в миллисекундах: $$W=\{72,59,7,34,59,76,64,90,56\}$$ Маршрутизаторы, имеют горячее резервирование кратности: $$SR=\{2\}$$ Линии связи имеют дублирование кратности: $$SP=\{2\}$$ Требуется найти $r=2$ зарезервированных канала от узла $k=6$ к узлу $l=9$, не задействующих совместно незадублированные маршрутизаторы и линии связи, обеспечивающих суммарно минимальное время задержки.

![](Lab3Task_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


```r
# Grab all simple paths, use igraph :P
non_loop_paths <- all_simple_paths(g, from = k, to = l)
length(non_loop_paths)
```

```
## [1] 1
```


```r
# Define fuctions to calculate time delay
calculate_time_delay <- function(path, graph) {
    edge_indices <- t(combn(path, 2)) # Get all possible combinations & transpose it to set each combination as row
    edge_weights <- sapply(1 : (nrow(edge_indices)), function(j) {
        edge <- get.edge.ids(graph, edge_indices[j, ])
        if (length(edge) > 0) {
            return(E(graph)$weight[edge]) # Get current weight of edge 
        } else {
            return(0)
        }
    })
    return(sum(unlist(edge_weights))) # Sum all edges
}
```


```r
# Call this function  
time_delays <- sapply(non_loop_paths, calculate_time_delay, graph = g)
length(time_delays)
```

```
## [1] 1
```


```r
# Create limitations
N <- length(non_loop_paths) # Num of solutions
Fun <- c(time_delays) # Target functions, x_{t} means x'th solutions
A <- matrix(0, nrow = 1 + p + q, ncol = N) # A for limitations
B <- c(r) # Right parts of limitations
A[1, ] <- 1 # 1'st limitation - sum of all routs == r (number of backup routes)
```


```r
# Set limitations for communication line delay
# Routers place by rows, routs place by columns
# Set 1 in column if this route use this routers
set_SR <- function(route, A, i) {
    A[route + 1, i] <<- 1
}
```


```r
# Invoke this function
bim <- mapply(set_SR, non_loop_paths, i = 1 : N, MoreArgs = list(A = A))
B <- c(B, V(g)$standby)
```


```r
# Define function to set limitations for reservation of communication lines
# edges place by rows, routs place by columns
# Set 1 in column if this route use this edge 
set_SP <- function(route, A, i, g) {
    edge_list <- sapply(1 : (length(route) - 1), function(j) {
        if (j == length(route) - 1) { # So hock off cause this loop do not stop  
            return(NULL)
        }
        vertex1 <- route[j + 1] # route[1] = NA, route[2] = 1'st vertex, for some reason...
        vertex2 <- route[j + 2]
        edge <- get.edge.ids(g, c(vertex1, vertex2))
        return(edge)
    })
    edge_list <- unlist(edge_list)
    A[p + 1 + edge_list, i] <<- 1
}
```


```r
# Invoke this function
bim <- mapply(set_SP, non_loop_paths, i = 1 : N, MoreArgs = list(A = A, g = g))
B <- c(B, E(g)$standby)

B
```

```
##  [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
```


```r
# Set signs of inequality
CD <- c("=", rep("<=", nrow(A) - 1))
CD
```

```
##  [1] "="  "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<="
## [16] "<=" "<=" "<=" "<="
```


```r
# Solve this task using lpSolve
optimum <- lp(
  direction = "min",
  objective.in = Fun,
  const.mat = A,
  const.dir = CD,
  const.rhs = B,
  all.bin = TRUE
)

optimum
```

```
## Error: no feasible solution found
```

```r
optimum$solution
```

```
## [1] 0
```


```r
# Show ids of routs that we choose 
# & whole rotes additionally
routes_id <- which(optimum$solution == 1)
routes_id
```

```
## integer(0)
```

```r
for(i in routes_id){
    cat(non_loop_paths[[i]])
    cat("\n")
}
```


```r
# Define function to plot solutions
plot_graph_with_routes <- function(graph, selected_routes, special_nodes) {
    V(graph)$color <- "orange" # Useless vertices in this solution
    V(graph)$color[special_nodes] <- "black" # Useless edges in this solution
    E(graph)$color <- "white" # Target vertices
    edge_list <- c()
    for (j in 1:(length(selected_routes[[1]]) - 1)) {
        vertex1 <- selected_routes[[1]][j] # For some reason here count starts with 1, not 2... 
        vertex2 <- selected_routes[[1]][j + 1]
        edge <- get.edge.ids(graph, c(vertex1, vertex2))
        edge_list <- c(edge_list, edge)
        intermediate_vertices <- selected_routes[[1]][-c(1, length(selected_routes[[1]]))]
        V(graph)$color[intermediate_vertices] <- "green" # Set intermediate vertices
    }
    E(graph)$color[edge_list] <- "red" # Set used edges as red 
    
    # Set the color of text inside vertices to white if the vertex color is black
    V(graph)$label.color <- ifelse(V(graph)$color == "black", "white", "black")
    
    # Set the color of labels on unused edges to white
    E(graph)$label.color <- ifelse(E(graph)$color == "white", "white", "black")
    
    plot(graph, layout = layout.circle, edge.label = E(graph)$weight)
}
```


```r
# Plot the original graph
plot(g, layout = layout.circle, edge.label = E(g)$weight)
```

![](Lab3Task_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

```r
# Plot all necessary routs
for (i in routes_id) {
    plot_graph_with_routes(g, list(non_loop_paths[[i]]), v)
    title <- paste("Route", i, "cost -", time_delays[i])
    title(main = title)
}
```

# Оценивание

4-6 Поиск маршрутов вручную, задание ограничений вручную, простой вариант.

6-8 Автоматический поиск маршрутов, автоматическое или ручное задание ограничений, простой вариант.

8-10 Автоматическое решение общей задачи, отсутсвие явных циклов (используйте функции \*apply), продвинутая индексация.
