###########
# Nombre:    FuncionesJuegos.R
# Objetivo:  funciones para los algoritmos de teor?a de juegos vistos en clase
# Version:   1.0, Mon Nov 21 17:31 2016
# ToDo:      - Ampliar m?s
# Autor:     Alfredo Rodr?guez S?nchez
###########


# Equilibrios de Nash en estrategias mixtas
enash_em <- function(juego){
  
  # TOLERANCIA <- 0.0001
  TOLERANCIA <- 0.001
  tampunto<- 0.2
  m1 <- juego$utilidad_J1
  m2 <- juego$utilidad_J2
  e1 <- juego$estrategias_J1
  e2 <- juego$estrategias_J2
  
  if(!(length(e1)==2 && length(e2)==2)){
    cat("Funcion no disponible para juegos de dimension distinta a 2x2\n")
    return
  }
  
  f1 <- paste(m1[1,1],"*p*q+",m1[1,2],"*p*(1-q)+",m1[2,1],"*(1-p)*q+",m1[2,2],"*(1-p)*(1-q)")
  p1 = function(p,q)  eval(parse(text = f1)) 
  
  f2 <- paste(m2[1,1],"*p*q+",m2[1,2],"*p*(1-q)+",m2[2,1],"*(1-p)*q+",m2[2,2],"*(1-p)*(1-q)")
  p2 = function(p,q)  eval(parse(text = f2)) 
  
  #dev.off()
  plot(-99,-99,axes=FALSE,xlab="p",ylab="q",xlim=c(0,1.1),ylim=c(0,1.4),main="Funciones de reaccion (ENEM)",
       sub="(Rojo) Funcion de reaccion numero 1                 (Azul) Funcion de reaccion numero 2")
  axis(1,lwd=0)
  axis(2,lwd=0)
  
  lines(c(-0.01,(1.1)),c(0,0),type="l",col="black", lty=1)
  lines(c(0,0),c(-0.01,(1.2)),type="l",col="black", lty=1)
  
  # print(f1)
  # print(f2)
  # aaaaaaaa
  
  v <- seq(0,1,by=0.001)
  v2 <- v
  v <- c(v,1/3,2/3)
  v2 <- c(v2,1/3,2/3)
  
  
  huecos <- matrix(rep(0,length(v)*length(v2)),ncol=length(v))
  
  # print(v2)
  for(q in v2){
    maxi <- max(p1(v2,q))
    aux <- p1(v2,q)
    aux <- which(abs(aux-maxi)<TOLERANCIA)
    points(v2[aux],rep(q,length(aux)),col="red",pch=22,cex=tampunto)
    
    aux2 <- which(v2==q)
    #print(aux)
    #print(rep(aux2,length(aux)))
   
    huecos[aux,rep(aux2,length(aux))] <- huecos[aux,rep(aux2,length(aux))] + 1
  }
  
  for(p in v){
    maxi <- max(p2(p,v))
    aux <- p2(p,v)
    aux <- which(abs(aux-maxi)<TOLERANCIA)
    points(rep(p,length(aux)),v[aux],col="blue",pch=22,cex=tampunto)
    
    aux2 <- which(v==p)
    huecos[rep(aux2,length(aux)),aux] <- huecos[rep(aux2,length(aux)),aux] + 1
    
  }
  
  aux <- which(huecos==2, arr.ind = TRUE)
  
  yahechos <- c()
  for(i in 1:length(aux[,1])){
    emp <- v[aux[i,1]]
    emq <- v2[aux[i,2]]
    empr <- round(emp,2)
    emqr <- round(emq,2)
    act <- paste0(empr,"_",emqr)
    if(!is.element(act,yahechos)){
      yahechos <- c(yahechos,act)
      points(emp,emq,col="green",pch=13,lwd=2,cex=tampunto*10)
      #print(aux[i,1])
      #print(aux[i,2])
      cat("Equilibrio de Nash en EM en (p=",emp,",q=",emq,")\n",sep="")
      cat("   >> ENEM = (",emp,"*",e1[1],"+",(1-emp),"*",e1[2],",",emq,"*",e2[1],"+",(1-emq),"*",e2[2],")\n",sep="")
    }
  }
  
  #legend(0.2,1.5,c("Func.Reacc.#1","Func.Reacc.#2"), lty = c(1,1),
  #       col=c('red','blue'),bty ="n", horiz=TRUE, cex=0.8, lwd=2.2)
  
  
  #return(aux)
  
}

# Equilibrios de Nash en estrategias puras
enash_ep <- function(juego){
  m1 <- juego$utilidad_J1
  m2 <- juego$utilidad_J2
  e1 <- juego$estrategias_J1
  e2 <- juego$estrategias_J2
  
  n1 <- m1*0
  n2 <- n1
  
  for(i in 1:length(e1)){
    maxi <- max(m2[i,])
    for(j in 1:length(e2)){
      if(m2[i,j]==maxi){
        n2[i,j] <- 1
      }
    }
  }
  for(j in 1:length(e2)){
    maxi <- max(m1[,j])
    for(i in 1:length(e1)){
      if(m1[i,j]==maxi){
        n1[i,j] <- 1
      }
    }
  }
  
  
  for(i in 1:length(e1)){
    for(j in 1:length(e2)){
      if(n1[i,j]==1){
        m1[i,j] <- paste("__",m1[i,j],"__",sep="")
      }
      if(n2[i,j]==1){
        m2[i,j] <- paste("__",m2[i,j],"__",sep="")
      }
    }
  }
  
  juego$utilidad_J1 <- m1
  juego$utilidad_J2 <- m2
  muestra_juego(juego)
  
  encontrados <- 0
  for(i in 1:length(e1)){
    for(j in 1:length(e2)){
      if((n1[i,j]==1) && (n2[i,j]==1)){
        cat("Equilibrio de Nash en EP en (",e1[i],",",e2[j],")\n",sep="")
        encontrados <- encontrados + 1
      }
    }
  }
  if(encontrados==0){
    cat("Ning?n Equilibrio de Nash en EP encontrado","\n",sep="")
  }
  
  
}


# Si todos los de a son mayores que los de b devuelve un 1, si son todos menores
# devuelve un 2 y si no 0
auxiliar_compara <- function(a,b){
  todos_mayores <- 1
  todos_menores <- 1
  for(i in 1:length(a)){
    if(a[i]<=b[i]){
      todos_mayores <- 0
    }
    if(a[i]>=b[i]){
      todos_menores <- 0
    }
    
  }
  if(todos_mayores>0){
    return(1)
  }
  if(todos_menores>0){
    return(2)
  }
  return(0)
}

auxiliar_quita_columna <- function(juego,c){
  
  
  m1 <- juego$utilidad_J1
  m2 <- juego$utilidad_J2
  
  e1 <- juego$estrategias_J1
  e2 <- juego$estrategias_J2
  
  m1 <- m1[,-c,drop=FALSE]
  m2 <- m2[,-c,drop=FALSE]
  e2 <- e2[-c]
  
  nj <- list(utilidad_J1=m1,utilidad_J2=m2,estrategias_J1=e1,estrategias_J2=e2)
  
  
  return(nj)
  
}

auxiliar_quita_fila <- function(juego,f){
  
  
  m1 <- juego$utilidad_J1
  m2 <- juego$utilidad_J2
  
  e1 <- juego$estrategias_J1
  e2 <- juego$estrategias_J2
  
  m1 <- m1[-f,,drop=FALSE]
  m2 <- m2[-f,,drop=FALSE]
  e1 <- e1[-f]
  
  nj <- list(utilidad_J1=m1,utilidad_J2=m2,estrategias_J1=e1,estrategias_J2=e2)
  
  
  return(nj)
  
}

# quita estrategias estrictamente dominadas
quita_eed <- function(juego,em){
  
  m1 <- juego$utilidad_J1
  m2 <- juego$utilidad_J2
  
  e1 <- juego$estrategias_J1
  e2 <- juego$estrategias_J2
  
  for(i in 1:nrow(m1)){
    for(j in 1:nrow(m1)){
      if(i!=j){
        aux <- auxiliar_compara(m1[i,],m1[j,])
        if(aux>0){
          if(aux==1){ f=j }
          else { f=i }
          cat("Quito la fila ",f," al estar estrictamente dominada por la fila ",i,"\n",sep="")
          return(quita_eed(auxiliar_quita_fila(juego,f),em))
        }
      }
    }
  }
  for(i in 1:ncol(m2)){
    for(j in 1:ncol(m2)){
      if(i!=j){
        aux <- auxiliar_compara(m2[,i],m2[,j])
        if(aux>0){
          if(aux==1){ c=j }
          else { c=i }
          cat("Quito la columna ",c," al estar estrictamente dominada por la columna ",i,"\n",sep="")
          return(quita_eed(auxiliar_quita_columna(juego,c),em))
        }
      }
    }
  }
  
  
  # Estrategias mixtas
  if(em){
    
    for(i in 1:nrow(m1)){
      for(j in 1:nrow(m1)){
        for(k in 1:nrow(m1)){
          if((i!=j) && (i!=k) && (k!=j)){
            for(p in seq(0,1,by=0.01)){
              aux <- auxiliar_compara(m1[i,]*p+m1[j,]*(1-p),m1[k,])
              if(aux==1){ 
                cat("Quito la fila ",k," al estar dominada por al menos una estrategia mixta:\n",sep="")
                cat("   >> Ejemplo: (Fila ",i,")*(",p,")+(Fila ",j,")*(",(1-p),")\n",sep="")
                return(quita_eed(auxiliar_quita_fila(juego,k),em))
              }
            }
          }
        }
      }
    }
    
    for(i in 1:ncol(m2)){
      for(j in 1:ncol(m2)){
        for(k in 1:ncol(m2)){
          if((i!=j) && (i!=k) && (k!=j)){
            for(p in seq(0,1,by=0.01)){
              aux <- auxiliar_compara(m2[,i]*p+m2[,j]*(1-p),m2[,k])
                if(aux==1){ 
                cat("Quito la columna ",k," al estar dominada por al menos una estrategia mixta:\n",sep="")
                cat("   >> Ejemplo: (Columna ",i,")*(",p,")+(Columna ",j,")*(",(1-p),")\n",sep="")
                return(quita_eed(auxiliar_quita_columna(juego,k),em))
                }
            }
          }
        }
      }
    }
    
  }
  
  
  return(juego)
  
}




# Muestra de forma ordenada la informaci?n correspondiente al juego
# por pantalla
muestra_juego <- function(juego){
  
  cat("\n")
  m1 <- juego$utilidad_J1
  m2 <- juego$utilidad_J2
  e1 <- juego$estrategias_J1
  e2 <- juego$estrategias_J2
  
  l <- ''
  blanco <- ' '
  # Calculamos el ancho del juego
  aux <- as.character(m1)
  anchoj1 <- max(nchar(aux))
  aux <- as.character(m2)
  anchoj2 <- max(nchar(aux))
  ancho <- max(anchoj1,anchoj2)
  
  anchoe1 <- max(nchar(e1))
  anchoe2 <- max(nchar(e2))
  # print(anchoe1)
  # print(e1)
  for(i in 1:(nchar("Jugador 1")+1+1+anchoe1+1)){
    l <- paste(l," ",sep="")
  }
  anchojuego <- (1+(1+ancho+1+ancho+1+1)*length(e2))
  aux <- round((anchojuego-nchar("Jugador 2"))/2)
  #print(anchojuego)
  for(i in 1:aux){
    l <- paste(l," ",sep="")
  }
  l <- paste(l,"Jugador 2",sep="")
  for(i in 1:(anchojuego-aux)){
    l <- paste(l," ",sep="")
  }
  l <- paste(l,"\n",sep="")
  
  
  for(i in 1:(nchar("Jugador 1")+1+1+anchoe1+1)){
    l <- paste(l," ",sep="")
  }
  for(i in 1:anchojuego){
    l <- paste(l,"-",sep="")
  }
  l <- paste(l,"\n",sep="")
  
  
  for(i in 1:(nchar("Jugador 1")+1+anchoe1+1)){
    l <- paste(l," ",sep="")
  }
  for(i in e2){
    l <- paste(l,auxiliar_huecos_delante(i,2*ancho+4),sep="")
    
  }
  
  l <- paste(l,"\n",sep="")
    
  for(i in 1:(nchar("Jugador 1")+1+anchoe1+1+1)){
    l <- paste(l," ",sep="")
  }
  for(i in 1:anchojuego){
    l <- paste(l,"-",sep="")
  }
  l <- paste(l,"\n",sep="")
  
  nlin <- 1
  ltot <- length(e1)*2
  lcen <- 1 #round(ltot/2)
  lin <- 1
  for(e in e1){
    if(nlin==lcen){
      l <- paste(l,"Jugador 1 ",sep="")
    }
    else{
      for(i in 1:(nchar("Jugador 1")+1)){
        l <- paste(l," ",sep="")
      }
    }
    l <- paste(l,auxiliar_huecos_delante(e,anchoe1),sep="")
    l <- paste(l," ",sep="")
    
    for(j in 1:length(e2)){
      l <- paste(l," | ",sep="")
      l <- paste(l,auxiliar_huecos_delante(as.character(m1[lin,j]),ancho),sep="")
      l <- paste(l,",",sep="")
      l <- paste(l,auxiliar_huecos_delante(as.character(m2[lin,j]),ancho),sep="")
      
    }
    l <- paste(l," | ",sep="")
    
    
    l <- paste(l,"\n",sep="")
    
    nlin <- nlin + 1
    
    for(i in 1:(nchar("Jugador 1")+1+anchoe1+2)){
      l <- paste(l," ",sep="")
    }
    for(i in 1:anchojuego){
      l <- paste(l,"-",sep="")
    }
   
    
    
    l <- paste(l,"\n",sep="")
    
    nlin <- nlin + 1
    lin <- lin + 1
  }

  l <- paste(l,"\n",sep="")
  
  cat(l)
  
    
}


auxiliar_huecos_delante<- function(c,n){
  l <- nchar(c)
  r <- ''
  #print(c)
  #print(l)
  #print(n)
  #r <- paste(r,"@",sep="")
  if((n-l)>0){
  for(i in 1:(n-l)){
    r <- paste(r," ",sep="")
  }
  }
  r <- paste(r,c,sep="")
  return(r)
  
  
}


auxresuelve <- function(t,vari,valor,guess){
  options(warn=-1)
  aux <- t
  aux <- gsub(vari, valor, aux)
  
  pr <- paste("
              zfunc<-function(x) {
              z<-vector()
              z[1]<- ",aux ,"
              z
              }",sep="");

  eval(parse(text=pr))
  
  known=list()
  sink("ksolve_log.txt")
  sol <- ktsolve(zfunc,known=known,guess=guess,show=FALSE)
  sink() #end diversion of output
  
  aux <- sol$results$par[1]
  options(warn=0)
  return(as.numeric(aux))
}


bertrand <- function(q1,q2,ct1,ct2,secuencial){
  
  # ToDo: No usar el D( que a veces falla y usar Deriv
  # Para resolver el sistema de ecuaciones de las derivadas primeras
  require(ktsolve)
  require(BB)
  require(Deriv)
  options(warn=-1)

  margen <- 1.4
  
  cat("\n")
  cat("Duopolio de Bertrand")
 
  b1 <- paste("(",q1,")*p1-(",ct1,")",sep="")
  b1 <- gsub("q1", paste("(",q1,")",sep=""), b1)
  t <- paste('D(expression(',b1,'),c("p1"))',sep="")
  db1 <- eval(parse(text=t))
  
  b2 <- paste("(",q2,")*p2-(",ct2,")",sep="")
  b2 <- gsub("q2", paste("(",q2,")",sep=""), b2)
  t <- paste('D(expression(',b2,'),c("p2"))',sep="")
  db2 <- eval(parse(text=t))
  
  
  if(secuencial){
    
    cat(" (decisi?n secuencial)\n")
    # Como no puedo despejar doy un par de valores y saco la recta
    # solo valdr? para funciones con primera derivada lineal
    x1 <- 0
    y1 <- auxresuelve(deparse(db2),"p1",x1,list(p2=0))
    x2 <- 100
    y2 <- auxresuelve(deparse(db2),"p1",x2,list(p2=0))
    
    #q2s_cad <- paste("( ((q1-(",x1,"))/(",x2-x1,"))*(",y2-y1,")+",y1,")",sep="")
    
    auxb <- 0-((x1*(y2-y1))/(x2-x1))
    auxb <- auxb+y1
    if(auxb<0){
      auxb <- paste("0",auxb,sep="")
    }
    auxa <- (y2-y1)/(x2-x1)
    if(auxa<0){
      auxa <- paste("0",auxa,sep="")
    }
    
    p2s_cad <- paste("((",auxa,")*p1+(",auxb,"))",sep="")
    
    aux<-b1
    aux <- gsub("p2",p2s_cad,aux)
    
    db1<- Deriv(aux, "p1") # differentiate
    
    
    pr <- paste("
                zfunc<-function(x) {
                z<-vector()
                z[1]<- ",db1 ,"
                z
                }",sep="");

    eval(parse(text=pr))
    
    known=list()
    guess=list(p1=0)
    sink("ksolve_log.txt")
    sol <- ktsolve(zfunc,known=known,guess=guess)
    sink() #end diversion of output
    p1s <- as.numeric(sol$results$par[1])
    
    p2s<-gsub("p1",p1s,p2s_cad)
    p2s<-eval(parse(text=p2s))
    
    f1x <- c(p1s,p1s)
    f1y <- c(0,p2s*5)
    
  }else{
    
    cat(" (decisi?n simult?nea)\n")
    
  
  
    pr <- paste("
                zfunc<-function(x) {
                z<-vector()
                z[1]<- ",deparse(db1) ,"
                z[2]<- ",deparse(db2),"
                z
                }",sep="");
  
    eval(parse(text=pr))
    
    known=list()
    guess=list(p1=0,p2=0)
    sink("ksolve_log.txt")
    sol <- ktsolve(zfunc,known=known,guess=guess)
    sink() #end diversion of output
    p1s <- as.numeric(sol$results$par[1])
    p2s <- as.numeric(sol$results$par[2])
    
    
    
    f1x <- c()
    f1y <- c()
    
    for(i in (-10):(max(p1s,p2s)*2)){
      f1x <- c(f1x,i)
      f1y <- c(f1y,auxresuelve(deparse(db1),"p1",paste("",i,sep=""),list(p2=0)) )
    }
  }
  
  f2x <- c()
  f2y <- c()
  
  for(i in (-10):(max(p1s,p2s)*2)){
    f2x <- c(f2x,i)
    f2y <- c(f2y,auxresuelve(deparse(db2),"p1",paste("",i,sep=""),list(p2=0)) )
  }
  
  
  
  plot(f1x,f1y,type="l",col="red",axes=FALSE,lwd=2.2,xlab="p1",ylab="p2",main="Funciones de reacci?n (Bertrand)",xlim=c(0,p1s*margen),ylim=c(0,p2s*margen),
       sub="(Rojo) Funci?n de reacci?n n?mero 1                 (Azul) Funci?n de reacci?n n?mero 2")
  axis(1,lwd=0)
  axis(2,lwd=0)
  
  lines(f2x,f2y,type="l",col="blue",lwd=2.2)
  
  lines(c(-1,round(p1s*margen)),c(0,0),type="l",col="black", lty=1)
  lines(c(0,0),c(-1,round(p2s*margen)),type="l",col="black", lty=1)
  lines(c(0,p1s),c(p2s,p2s),type="b",col="black", lty=2)
  lines(c(p1s,p1s),c(0,p2s),type="b",col="black", lty=2)
  points(p1s,p2s,col="green",pch=13,lwd=2,cex=2)
  
  #legend(0,p2s*1.4,c("Func.Reacc.#1","Func.Reacc.#2"), lty = c(1,1),
  #       col=c('red','blue'),bty ="n", horiz=TRUE, cex=0.8, lwd=2.2)
  
  # Calculamos los beneficios
  
  aux <- b1
  aux <- gsub("p1", p1s, aux)
  aux <- gsub("p2", p2s, aux)
  aux <- paste("benef1<-(",aux,")",sep="")
  eval(parse(text=aux))
  
  aux <- b2
  aux <- gsub("p1", p1s, aux)
  aux <- gsub("p2", p2s, aux)
  aux <- paste("benef2<-(",aux,")",sep="")
  eval(parse(text=aux))
  
  cat("Precios de equilibrio:\n")
  cat("  >> p1*=",p1s,"\n")
  cat("  >> p2*=",p2s,"\n\n")
  
  cat("Beneficios:\n")
  cat("  >> beneficio1=",benef1,"\n")
  cat("  >> beneficio2=",benef2,"\n\n")
  
}


cournot <- function(p1,p2,ct1,ct2,secuencial){
  
  # ToDo: No usar el D( que a veces falla y usar Deriv
  # Para resolver el sistema de ecuaciones de las derivadas primeras
  require(ktsolve)
  require(BB)
  require(Deriv)
  options(warn=-1)
  
  margen <- 1.4
  
  cat("\n")
  cat("Duopolio de Cournot")

  
  
  
  b1 <- paste("(",p1,")*q1-(",ct1,")",sep="")
  #b1 <- gsub("p1", paste("(",p1,")",sep=""), b1)
  
  t <- paste('D(expression(',b1,'),c("q1"))',sep="")
  db1 <- eval(parse(text=t))
  
  b2 <- paste("(",p2,")*q2-(",ct2,")",sep="")
  #b2 <- gsub("p2", paste("(",p2,")",sep=""), b1)
  
  t <- paste('D(expression(',b2,'),c("q2"))',sep="")
  db2 <- eval(parse(text=t))
  
  
  if(secuencial){
    
    cat(" (decisi?n secuencial)\n")
    # Como no puedo despejar doy un par de valores y saco la recta
    # solo valdr? para funciones con primera derivada lineal
    x1 <- 0
    y1 <- auxresuelve(deparse(db2),"q1",x1,list(q2=0))
    x2 <- 100
    y2 <- auxresuelve(deparse(db2),"q1",x2,list(q2=0))
    
    #q2s_cad <- paste("( ((q1-(",x1,"))/(",x2-x1,"))*(",y2-y1,")+",y1,")",sep="")
    
    auxb <- 0-((x1*(y2-y1))/(x2-x1))
    auxb <- auxb+y1
    if(auxb<0){
      auxb <- paste("0",auxb,sep="")
    }
    auxa <- (y2-y1)/(x2-x1)
    if(auxa<0){
      auxa <- paste("0",auxa,sep="")
    }
    
    q2s_cad <- paste("((",auxa,")*q1+(",auxb,"))",sep="")
    
    aux<-b1
    aux <- gsub("q2",q2s_cad,aux)

    db1<- Deriv(aux, "q1") # differentiate only by x

    
    pr <- paste("
                zfunc<-function(x) {
                z<-vector()
                z[1]<- ",db1 ,"
                z
                }",sep="");

    eval(parse(text=pr))
    
    known=list()
    guess=list(q1=0)
    sink("ksolve_log.txt")
    sol <- ktsolve(zfunc,known=known,guess=guess)
    sink() #end diversion of output
    q1s <- as.numeric(sol$results$par[1])
    
    q2s<-gsub("q1",q1s,q2s_cad)
    q2s<-eval(parse(text=q2s))

    f1x <- c(q1s,q1s)
    f1y <- c(0,q2s*5)
    
  }else{
    
    cat(" (decisi?n simult?nea)\n")

    pr <- paste("
                zfunc<-function(x) {
                z<-vector()
                z[1]<- ",deparse(db1) ,"
                z[2]<- ",deparse(db2),"
                z
                }",sep="");
  
    eval(parse(text=pr))
    
    known=list()
    guess=list(q1=0,q2=0)
    sink("ksolve_log.txt")
    sol <- ktsolve(zfunc,known=known,guess=guess)
    sink() #end diversion of output
    q1s <- as.numeric(sol$results$par[1])
    q2s <- as.numeric(sol$results$par[2])

    f1x <- c()
    f1y <- c()
    
    for(i in (-10):(max(q1s,q2s)*2)){
      f1x <- c(f1x,i)
      f1y <- c(f1y,auxresuelve(deparse(db1),"q1",paste("",i,sep=""),list(q2=0)) )
    }
  
  }
  
  
  f2x <- c()
  f2y <- c()
  
  for(i in (-10):(max(q1s,q2s)*2)){
    f2x <- c(f2x,i)
    f2y <- c(f2y,auxresuelve(deparse(db2),"q1",paste("",i,sep=""),list(q2=0)) )
  }
  
  
  
  plot(f1x,f1y,type="l",col="red",
       axes=FALSE,lwd=2.2,xlab="q1",ylab="q2",
       main="Funciones de reacci?n (Cournot)",
       xlim=c(0,q1s*margen),ylim=c(0,q2s*margen),
       sub="(Rojo) Funci?n de reacci?n n?mero 1                 (Azul) Funci?n de reacci?n n?mero 2")
  axis(1,lwd=0)
  axis(2,lwd=0)
  
  lines(f2x,f2y,type="l",col="blue",lwd=2.2)
  
  lines(c(-1,round(q1s*margen)),c(0,0),type="l",col="black", lty=1)
  lines(c(0,0),c(-1,round(q2s*margen)),type="l",col="black", lty=1)
  lines(c(0,q1s),c(q2s,q2s),type="b",col="black", lty=2)
  lines(c(q1s,q1s),c(0,q2s),type="b",col="black", lty=2)
  points(q1s,q2s,col="green",pch=13,lwd=2,cex=2)
  
  #legend(0,q2s*1.4,c("Func.Reacc.#1","Func.Reacc.#2"), lty = c(1,1),
  #       col=c('red','blue'),bty ="n", horiz=TRUE, cex=0.8, lwd=2.2)
  
  # Calculamos los beneficios
  
  aux <- b1
  aux <- gsub("q1", q1s, aux)
  aux <- gsub("q2", q2s, aux)
  aux <- paste("benef1<-(",aux,")",sep="")
  eval(parse(text=aux))
  
  aux <- b2
  aux <- gsub("q1", q1s, aux)
  aux <- gsub("q2", q2s, aux)
  aux <- paste("benef2<-(",aux,")",sep="")
  eval(parse(text=aux))
  
  cat("Cantidades de equilibrio:\n")
  cat("  >> q1*=",q1s,"\n")
  cat("  >> q2*=",q2s,"\n\n")
  
  cat("Beneficios:\n")
  cat("  >> beneficio1=",benef1,"\n")
  cat("  >> beneficio2=",benef2,"\n\n")
  
  options(warn=0)
}










