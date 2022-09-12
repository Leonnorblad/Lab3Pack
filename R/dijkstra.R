#' Dijkstra
#'
#' Finds the shortest distance to each vertex in the graph from the initial node.
#'
#' @param graph A \code{data.frame} with three numeric variables named \code{v1}, \code{v2} and \code{w}. \code{v1} and \code{v2} are the graph edges, and  \code{w} is the corresponding weight of the edges.
#' @param init_node A numeric scalar that exist in the graph.
#' 
#' 
#' @return A vector containing distances of the shortest path between the initial node and each vertices in order.
#'
#' @examples
#' graph <- data.frame(v1 = c(1,1,1,2,2,2,2,3,3,3,4,4,5,5,6,6),
#' v2 = c(2,3,4,1,3,4,6,1,2,5,1,2,3,6,2,5),
#' w = c(5,3,10,5,7,2,14,3,7,1,10,2,1,7,14,7))
#' dijkstra(graph, 1)
#'
#' @references
#' Wikipedia contributors. (2022, August 21). Dijkstra's algorithm. In Wikipedia, The Free Encyclopedia. Retrieved 12:39, September 9, 2022, from \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
#' @export

dijkstra <-
function(graph, init_node){
    
    # checks the input
    if (init_node>max(graph$v1)|init_node<min(graph$v1) | # init_node is in grapth
        !is.numeric(init_node)|  # init_node has to be a numeric value
        !is.data.frame(graph) |  # graph needs to be a data.frame
        any(!is.numeric(c(graph$v1, graph$v2, graph$w))) |  # "v1", "v2" and "w" has to be numeric
        ! any(colnames(graph) %in% c("v1", "v2", "w"))      # graph needs to be named: "v1", "v2" and "w"
    ) { stop()}
    
    # Create empty vectors 
    dist <- c()   # distance vector used to store distances between the initial node and other vertices 
    Q <- c()      # used to store vertices linked to a current vertex that have not been tried
    
    graph_vertices <- unique(graph$v1)  # stores respective vertices 
    for (v in graph_vertices) {       # the for loop used to assign initial values for every vertex in the graph 
        dist[v] <- Inf                  # dist[v] gets Inf and returns it if there are no neighbors to the current vertex
        Q[v] <- v                       # assigns all the vertices in the graph
    }
    
    dist[init_node] <- 0              # distance to the initial node is always zero
    
    while (length(Q) > 0) {           # the while loop will run as long as there are paths to be tried
        u <- Q[which.min(dist[Q])]      # vertex in Q with the minimum distance 
        Q <- Q[Q!=u]                    # removes the selected u from Q 
        
        neighbor <- graph$v2[graph$v1==u] # selecting the neighbors of u
        for (v in neighbor) {             # testing the length of all the paths to the neighbors of u
            alt <- dist[u] + graph$w[graph$v1==v & graph$v2==u]   # the alternative path is the previously selected path and the newly selected path 
            
            if (alt < dist[v]) {            # checks if the alternative path is shorter than the previously selected path
                dist[v] <- alt                # this overwrites the previously selected path with the new shorter path
            }
        }
    }
    return(dist)                        # returns a vector of the shortest distances (stored during the tests)
}
