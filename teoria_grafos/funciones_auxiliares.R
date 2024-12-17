
dijkstra <- function(a,p,d,origen){
  options(warn=-1)
  datos <- list(arcos=a,pesos=p,dirigido=d)
  if(is.null(datos$pesos)){
    print("El algoritmo de Dijkstra necesita un grafo con pesos")
    return(c("",""))
  }else{
    if(!is.element(origen,a)){
      print("El nodo de origen no existe")
      return(c("",""))
    }else{
      
      ng <- igraph::graph(a,n=max(a))
      if(!d){
        ng <- as.undirected(ng,mode="each")
      }
      if(!is.null(p)){
        E(ng)$label <- p
      }
      aux <- shortest.paths(ng, mode="out",weights=datos$pesos,algorithm="dijkstra")
      pp <- aux[origen,]
      aux <- shortest_paths(ng,1, mode="out",weights=datos$pesos)
      rr <- aux$vpath
      # return(list(pesos=pp,rutas=rr))
      
    
      for(i in 1:length(rr)){
        if(i==origen){ next }
        print(paste0("De ",origen," a ",i,": Ruta: ",
                     str_replace_all(str_replace_all(str_replace_all(rr[i],", "," > "),"\\)",""),"c\\(","")
                     ,", Coste minimo: ",pp[i]))
      }
    }
    
  }
  options(warn=0)
  
}
