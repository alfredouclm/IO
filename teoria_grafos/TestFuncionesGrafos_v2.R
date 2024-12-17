

setwd("~/SynologyDrive/UCLM ADE/Investigacion Operativa (2023-2024)/practicas/practica2/grafos_r")

library(igraph)
library(openxlsx)
library(stringr)

source("funciones_auxiliares.R")


# Nodos empiezan en 1
arcos <- c(1,3, 1,4, 1,5, 1,6, 2,3, 2,4,  6,4, 5,6)
pesos <- c(9,   22,  4,   5,   12,  1,   15,   2)
dirigido <- TRUE
ng <- igraph::graph(arcos,n=max(arcos))
# ng <- igraph::as.undirected(ng,mode="each")
E(ng)$label <- pesos
plot(ng)

m <- get.adjacency(ng,attr="label")
print(m)
dijkstra(arcos,pesos,dirigido,1)



# Ejercicio de examen
# Nodos empiezan en 1
arcos <- c(1,2,1,3,2,3,2,4,3,4,2,5,4,5,4,6,5,6)
pesos <- c(  1,  2,  1,  2,  4,  3,  1,  2,  3)


dirigido <- TRUE
ng <- igraph::graph(arcos,n=max(arcos))
# ng <- igraph::as.undirected(ng,mode="each")
# E(ng)$label <- pesos

# gimp <- igraph::graph_from_data_frame(drel,directed = T)
# clp <- cluster_label_prop(gimp)
# plot(clp, gimp)
# E(gimp)$weights <- drel$distancia[1:40]
# E(ng)$width <- ng$distancia/5

ANCHO_IMAGEN <- 1500
ALTO_IMAGEN <- 1500

FILE <- paste0("g2.png")
png(FILE,res=200,width=ANCHO_IMAGEN,height=ALTO_IMAGEN,units = "px")


# https://yunranchen.github.io/intro-net-r/igraph.html
# https://kateto.net/netscix2016.html
plot(ng,layout=layout.davidson.harel,edge.arrow.size=1, cex=2,
     # vertex.color="slategray2", 
     vertex.color = c("sienna2","slategray2","slategray2","slategray2","slategray2","sienna2"),
     vertex.size=25, 
     vertex.frame.color="black", vertex.label.color="black", vertex.label.font =1,
     edge.label.color="black", edge.label.cex=1.6,
     edge.color ="grey60",
     vertex.label.cex=2, vertex.label.dist=0, edge.curved=0.2,edge.label=round(pesos))


dev.off()

# m <- get.adjacency(ng,attr="label")
# print(m)


dijkstra(arcos,pesos,dirigido,1)



dexcel <- openxlsx::read.xlsx("distancias_toledo.xlsx")
dexcel <- subset(dexcel,lato>=39.8 & lono>=-3.96)
drel <- dexcel[,c(6,10,11)]
drel <- drel[1:30,]
# dv <- unique(dexcel[,6])


gimp <- igraph::graph_from_data_frame(drel,directed = T)

# clp <- cluster_label_prop(gimp)
# plot(clp, gimp)

# E(gimp)$weights <- drel$distancia[1:40]

E(gimp)$width <- drel$distancia/5
  
# https://yunranchen.github.io/intro-net-r/igraph.html
# https://kateto.net/netscix2016.html
plot(gimp,layout=layout.auto,edge.arrow.size=.2, vertex.color="gold", vertex.size=10, 
     vertex.frame.color="gray", vertex.label.color="black", 
     vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2,edge.label=round(drel$distancia))

# library(igraphdata)
# data("UKfaculty")
# 
# plot(UKfaculty,layout=layout.auto,edge.arrow.size=.2, vertex.color="gold", vertex.size=10, 
#      vertex.frame.color="gray", vertex.label.color="black", 
#      vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2)

