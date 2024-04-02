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
# install.packages('extraDistr')

library(digest)
library(extraDistr)
library(lpSolve)
library(igraph)

set.seed(digest2int('Sikolenko Mikchail Aleksandrovich'))
```

Переменная *simple* отвечает за выбор варианта сложности.


```r
simple <- F
```

Далее на основании вашего ФИО и варианта сложности генерируются основные параметры задачи.
Число маршрутизаторов $p$:


```r
p <- ifelse(simple, rdunif(1, 10, 10), rdunif(1, 25, 50))
p
```

```
## [1] 32
```

Число резервных маршрутов $r$


```r
r <- ifelse(simple,2,rdunif(1,4,5))
r
```

```
## [1] 4
```

Граф маршрутизации


```r
repeat {
g <<- sample_gnp(p, ifelse(simple,0.2,0.05))
if(is_connected(g))
  break
}
```

Количество линий связи $q$


```r
q <- length(E(g))
q
```

```
## [1] 39
```

Задержка линии связи $W$


```r
E(g)$weight <- rdunif(q,1,100)
```

Резервирование маршрутизаторов $SR$


```r
V(g)$standby <- if(simple) rep(2,p) else rdunif(p,r,5)
```

Резервирование линий связи $SP$


```r
E(g)$standby <- if(simple) rep(2,q) else rdunif(q,r,5)
```

Узлы, между которыми требуется обеспечить надежную связь


```r
v <- farthest_vertices(g)$vertices
v
```

```
## + 2/32 vertices, from ed272aa:
## [1] 11 25
```

```r
k <- v[1]
l <- v[2]
```

# Ваш вариант



Общая формулировка задачи выглядит следующим образом **(подстановка реализуется при сохранении, например html)**:

Рассмотрим телекоммуникационную сеть, состоящую из $p=32$ узлов-маршрутизаторов $R$ и $q=39$ соединяющих их линий связи $P$.
Сеть определяется граформ, заданным списком смежности: $$
\newcommand\ue{\mathrel{\bullet\mkern-3mu{-}\mkern-3mu\bullet}}
\{2\ue5,1\ue8,7\ue8,6\ue9,8\ue9,1\ue10,1\ue13,8\ue13,2\ue14,1\ue15,3\ue15,5\ue16,9\ue17,10\ue17,\\16\ue19,18\ue19,20\ue21,9\ue22,11\ue22,4\ue23,9\ue24,20\ue24,23\ue25,10\ue27,23\ue27,13\ue28,26\ue28,\\13\ue29,16\ue29,28\ue29,3\ue30,12\ue30,15\ue30,18\ue30,28\ue30,14\ue31,18\ue31,28\ue31,17\ue32\}
$$ Каждая линия связи оценивается временем задержки сигнала измеряемым в миллисекундах: $$W=\{25,92,81,97,36,95,8,24,31,95,52,10,97,70,7,93,68,40,83,10,16,23,24,84,69,11,56,37,31,97,13,56,12,7,2,\\27,79,52,11\}$$ Маршрутизаторы, имеют горячее резервирование кратности: $$SR=\{4,5,5,4,5,5,4,4,5,5,5,4,4,5,4,5,5,5,4,5,4,4,5,4,5,5,5,4,5,4,4,4\}$$ Линии связи имеют дублирование кратности: $$SP=\{5,4,5,4,5,4,5,4,4,5,4,4,5,5,4,4,4,5,4,4,5,4,4,4,5,5,5,4,5,5,4,4,5,5,5,4,5,4,4\}$$ Требуется найти $r=4$ зарезервированных канала от узла $k=11$ к узлу $l=25$, не задействующих совместно незадублированные маршрутизаторы и линии связи, обеспечивающих суммарно минимальное время задержки.

![](Lab3Task_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


```r
# edge_list <- as_edgelist(g)
# length(edge_list)
```


```r
# W <- E(g)$weight
# length(W)
```


```r
filter_loops <- function(paths) {
    filtered_paths <- lapply(paths, function(path) {
        if (!anyDuplicated(path)) {
            return(path)
        }
        return(NULL)
    })
    return(filtered_paths[!sapply(filtered_paths, is.null)])
}

find_non_loop_paths <- function(g, k, l) {
    all_paths <- all_simple_paths(g, from = k, to = l)
    non_loop_paths <- filter_loops(all_paths)
    return(non_loop_paths)
}
```


```r
non_loop_paths <- find_non_loop_paths(g, k, l)
length(non_loop_paths)
```

```
## [1] 27
```


```r
calculate_time_delay <- function(path, graph) {
    edge_indices <- t(combn(path, 2))
    edge_weights <- sapply(1:(nrow(edge_indices)), function(j) {
        edge <- get.edge.ids(graph, edge_indices[j, ])
        if (length(edge) > 0) {
            return(E(graph)$weight[edge])
        } else {
            return(0)
        }
    })
    return(sum(unlist(edge_weights)))
}
```


```r
time_delays <- sapply(non_loop_paths, calculate_time_delay, graph = g)
# time_delays
```


```r
N <- length(non_loop_paths)
Fun <- c(time_delays)
A <- matrix(0, nrow = 1 + p + q, ncol = N)
B <- c(r)
A[1, ] <- 1
```


```r
# A
```


```r
set_SR <- function(route, A, i) {
    A[route + 1, i] <<- 1
}

invisible(mapply(set_SR, non_loop_paths, i = 1 : N, MoreArgs = list(A = A)))
B <- c(B, V(g)$standby)

# A
```


```r
set_SP <- function(route, A, i, g) {
    edge_list <- sapply(1 : (length(route) - 1), function(j) {
        if (j == length(route) - 1) {
            return(NULL)
        }
        vertex1 <- route[j + 1]
        vertex2 <- route[j + 2]
        edge <- get.edge.ids(g, c(vertex1, vertex2))
        return(edge)
    })
    edge_list <- unlist(edge_list)
    A[p + edge_list + 1, i] <<- 1
}

invisible(mapply(set_SP, non_loop_paths, i = 1 : N, MoreArgs = list(A = A, g = g)))
B <- c(B, E(g)$standby)

# A
```


```r
CD <- c("=", rep("<=", nrow(A) - 1))
CD
```

```
##  [1] "="  "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<="
## [16] "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<="
## [31] "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<="
## [46] "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<="
## [61] "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<=" "<="
```



```r
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
## Success: the objective function is 2220
```

```r
optimum$solution
```

```
##  [1] 1 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
```


```r
routes_id <- which(optimum$solution == 1)
routes_id
```

```
## [1]  1  2  8 27
```

```r
for(i in routes_id){
    cat(non_loop_paths[[i]])
    cat("\n")
}
```

```
## 11 22 9 8 1 10 27 23 25
## 11 22 9 8 13 1 10 27 23 25
## 11 22 9 8 13 28 30 15 1 10 27 23 25
## 11 22 9 17 10 27 23 25
```


```r
plot_graph_with_routes <- function(graph, selected_routes, special_nodes) {
    V(graph)$color <- "orange"
    V(graph)$color[special_nodes] <- "black"
    E(graph)$color <- "black"
    edge_list <- c()
    for (j in 1:(length(selected_routes[[1]]) - 1)) {
        vertex1 <- selected_routes[[1]][j]
        vertex2 <- selected_routes[[1]][j + 1]
        edge <- get.edge.ids(graph, c(vertex1, vertex2))
        edge_list <- c(edge_list, edge)
        intermediate_vertices <- selected_routes[[1]][-c(1, length(selected_routes[[1]]))]
        V(graph)$color[intermediate_vertices] <- "green"
    }
    E(graph)$color[edge_list] <- "red"
    plot(graph, layout = layout.circle, edge.label = E(graph)$weight)
}
```


```r
for (i in routes_id) {
    plot_graph_with_routes(g, list(non_loop_paths[[i]]), v)
    title <- paste("Route", i, "cost -", time_delays[i])
    title(main = title)
}
```

![](Lab3Task_files/figure-html/unnamed-chunk-27-1.png)<!-- -->![](Lab3Task_files/figure-html/unnamed-chunk-27-2.png)<!-- -->![](Lab3Task_files/figure-html/unnamed-chunk-27-3.png)<!-- -->![](Lab3Task_files/figure-html/unnamed-chunk-27-4.png)<!-- -->


# Оценивание

4-6 Поиск маршрутов вручную, задание ограничений вручную, простой вариант.

6-8 Автоматический поиск маршрутов, автоматическое или ручное задание ограничений, простой вариант.

8-10 Автоматическое решение общей задачи, отсутсвие явных циклов (используйте функции \*apply), продвинутая индексация.
