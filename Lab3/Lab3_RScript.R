# Import list & set seed
# install.packages('extraDistr')

library(digest)
library(extraDistr)
library(lpSolve)
library(igraph)
library(stringr)

set.seed(digest2int('Sikolenko Mikchail Aleksandrovich'))


# Do difficult options
simple <- F


# Get number of routers
p <- ifelse(simple, rdunif(1, 6, 10), rdunif(1, 20, 50))
p


# Get number of backup routes
r <- ifelse(simple,2,rdunif(1,4,5))
r


# generate graph 
repeat {
g <<- sample_gnp(p, ifelse(simple,0.2,0.05))
if(is_connected(g))
  break
}


# Get number of communication lines
q <- length(E(g))
q


# Get communication line delay
E(g)$weight <- rdunif(q, 1, 100)


# Get Router redundancy
V(g)$standby <- if(simple) rep(2,p) else rdunif(p,r,5)


# Get reservation of communication lines
E(g)$standby <- if(simple) rep(2,q) else rdunif(q, r, 5)


# Get vertexes that we need to connect
v <- farthest_vertices(g)$vertices
v
k <- v[1]
l <- v[2]


# Generate LaTeX syntax option
latex_array <- \(x) gsub('(.{0,100})\\K,',',\\\\\\\\', paste(x, collapse = ',') |> paste0(',')  , perl=TRUE) |> str_sub(end = -4)
latex_adj_list <- \(g) g |> as_edgelist() |> apply(1, \(edg) edg |> paste(collapse = '\\ue')) |> latex_array()


# Plot graph
plot(g, layout = layout.circle, edge.label = E(g)$weight)


# Grab all simple paths, use igraph :P
non_loop_paths <- all_simple_paths(g, from = k, to = l)
length(non_loop_paths)


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


# Call this function  
time_delays <- sapply(non_loop_paths, calculate_time_delay, graph = g)
length(time_delays)


# Create limitations
N <- length(non_loop_paths) # Num of solutions
Fun <- c(time_delays) # Target functions, x_{t} means x'th solutions
A <- matrix(0, nrow = 1 + p + q, ncol = N) # A for limitations
B <- c(r) # Right parts of limitations
A[1, ] <- 1 # 1'st limitation - sum of all routs == r (number of backup routes)


# Set limitations for communication line delay
# Routers place by rows, routs place by columns
# Set 1 in column if this route use this routers
set_SR <- function(route, A, i) {
    A[route + 1, i] <<- 1
}


# Invoke this function
bim <- mapply(set_SR, non_loop_paths, i = 1 : N, MoreArgs = list(A = A))
B <- c(B, V(g)$standby)


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


# Invoke this function
bim <- mapply(set_SP, non_loop_paths, i = 1 : N, MoreArgs = list(A = A, g = g))
B <- c(B, E(g)$standby)

B


# Set signs of inequality
CD <- c("=", rep("<=", nrow(A) - 1))
CD


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
optimum$solution


# Show ids of routs that we choose 
# & whole rotes additionally
routes_id <- which(optimum$solution == 1)
routes_id

for(i in routes_id){
    cat(non_loop_paths[[i]])
    cat("\n")
}


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


# Plot the original graph
plot(g, layout = layout.circle, edge.label = E(g)$weight)

# Plot all necessary routs
for (i in routes_id) {
    plot_graph_with_routes(g, list(non_loop_paths[[i]]), v)
    title <- paste("Route", i, "cost -", time_delays[i])
    title(main = title)
}

