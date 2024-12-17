library(igraph) # usar la librería

# Dirigidos 
arcos <- c(1,4, 4,1, 1,5, 4,2, 5,3, 5,6, 2,6, 6,2)
pesos <- c(15,  15,  20,  25,  30,  10,  25,  25)
g <- graph(arcos, n=6 )
E(g)$label <- pesos  
plot(g)
get.adjacency(g,attr="label")
########################


# Sin dirigir y matriz de adyacencia
arcos <- c(1,3, 1,4, 1,5, 1,6, 2,3, 2,4,  6,4, 5,6)
pesos <- c(9,   22,  4,   5,   12,  1,   15,   2)
g <- graph(arcos, n=6 )
# Si lo queremos sin dirigir
g<-as.undirected(g,mode="each")
E(g)$label <- pesos  
plot(g)
get.adjacency(g,attr="label")
#######################


# Algoritmo de Dijkstra
arcos <- c(1,2, 1,3, 2,3, 3,4, 3,5, 4,2, 4,5)
pesos <- c(100, 30,  20,  10,  60,  15,  50)
g <- graph(arcos, n=5 )
E(g)$label <- pesos  
plot(g)
shortest.paths(g, mode="out",weights=pesos,algorithm="dijkstra")
get.shortest.paths(g, 1,2, weights=pesos)
#######################



library(optrees)

# Algoritmo árbol expansión mínima del ejemplo de los apuntes (otro resultado)
nodes <- 1:5
arcos <- matrix(c(1,2,1, 1,4,6, 1,5,2, 1,3,4, 5,4,4, 5,2,2, 5,3,2, 3,2,3 ),ncol=3,byrow=TRUE)
repGraph(nodes, arcos)
getMinimumSpanningTree(nodes,arcos,algorithm="Prim")
######################################


# Algoritmo del flujo máximo
nodos <- 1:5
arcos <- matrix(c(1,2,20, 1,4,10, 1,3,30, 2,3,40, 2,5,30, 3,4,5,
                  3,5,20, 4,5,20), ncol=3,byrow=T)
repGraph(nodos,arcos)
# Maximum flow with Ford-Fulkerson algorithm
maxFlowFordFulkerson(nodos, arcos, source.node = 1, sink.node = 5)
######################################



# Algoritmo de Dijkstra
nodes <- 1:5
arcs <- matrix(c(1,2,100, 1,3,30, 2,3,20, 3,4,10, 3,5,60, 4,2,15, 4,5,50),ncol=3,byrow=TRUE)
getShortestPathTree(nodes , arcs , algorithm = "Dijkstra",directed=F)
######################################





# 1 Toledo
# 2 Ciudad Real
# 3 Cuenca
# 4 Albacete
# 5 Guadalajara

n <- 1:5
a <- matrix(c(1,2,98, 1,3,163, 1,4,210, 1,5,113,
              2,3,196, 2,4,179, 2,5,195,
              3,4,122, 3,5,107)
              ,ncol=3,byrow=T)
repGraph(n, a)
getMinimumSpanningTree(n,a,algorithm="Prim")


