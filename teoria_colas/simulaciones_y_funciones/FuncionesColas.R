###########
# Nombre:    FuncionesColas.R
# Objetivo:  funciones para calcular las medidas de desempeño en colas M/M
# Version:   1.1, Fri Sep 02 12:20 2016
# ToDo:      - comprobar posibles errores
#            - añadir más funcionalidades
# Autores:   Originalmente creadas en el curso 2015/2016 por los alumnos:
#            - Estefanía Jiménez Arroyo
#            - Samuel López Hernández
#            - Lucía López-Rey Sánchez-Cano
#            * Permitieron su uso en cursos posteriores.
# Modif:     Alfredo Rodríguez Sánchez
#            - corrección de errores
#            - reestructuración del código
###########



###################
# Funciones M/M/1 #
###################

MM1_medidas<- function (lambda,mu){
  ro<- lambda/mu
  fi <- ro
  if(fi>=1){ 
    print(paste("El sistema no es estable: fi=",fi,sep=""))
    return(c(-99,-99,-99,-99,-99))
  }
  
  Ls<- ro/(1-ro)
  Lq<- (ro^2)/(1-ro)
  Ws<- 1/(mu-lambda)
  Wq<- ro/(mu*(1-ro))
  c<- ro
  vector<- c(Ls,Lq,Ws,Wq,c)
  return(vector)
}


MM1_pn <- function (lambda,mu,n) {
  ro <- lambda/mu
  fi <- ro
  if(fi>=1){ 
    print(paste("El sistema no es estable: fi=",fi,sep=""))
    return(-99)
  }
  pn <- (1 - ro)*(ro^n)
  return(pn)
}

MM1_PLsMayorN <- function (lambda,mu,n) {
  ro <- lambda/mu
  fi <- ro
  if(fi>=1){ 
    print(paste("El sistema no es estable: fi=",fi,sep=""))
    return(-99)
  }
  p <- ro^(n+1)
  return(p)
}


MM1_PWsMayorT <- function (lambda,mu,t){
  ro <- lambda/mu
  fi <- ro
  if(fi>=1){ 
    print(paste("El sistema no es estable: fi=",fi,sep=""))
    return(-99)
  }
  p <- exp((-mu)*(1-ro)*t)
  return(p)
  
}


MM1_PWqMayorT <- function (lambda,mu,t) {
  ro <- lambda/mu
  fi <- ro
  if(fi>=1){ 
    print(paste("El sistema no es estable: fi=",fi,sep=""))
    return(-99)
  }
  p <- ro*exp((-mu)*(1-ro)*t)
  return(p)
  
}



############################
# Funciones M/M/1:DG/N/inf #
############################

MM1N_pn <- function(lambda,mu,N,n){
  
  ro <- lambda/mu
  
  if (ro!=1) {
    pn <- ((1-ro)*(ro^n))/(1-ro^(N+1))
  } else {
    pn <- 1/(N+1)
  }
  return(pn)
}

MM1N_medidas <- function(lambda,mu,N) {
  
  ro <- lambda/mu
  
  if (ro!=1) {
    Ls <- (ro*(1-(N+1)*(ro^N) + N*(ro^(N+1)) )) / ((1-ro)*(1-ro^(N+1)))
  }else {
    Ls<-N/2
  }  
  
  # Probabilidad de que esté el sistema lleno (y entonces se pierden clientes)
  pN <- MM1N_pn(lambda,mu,N,N)
  
  # Con las fórmulas de Little
  lambdaperdida <- lambda*(pN)
  lambdaefectiva <- lambda - lambdaperdida
  Lq  <- Ls- (lambdaefectiva/mu)
  Ws <- Ls/lambdaefectiva
  Wq <- Lq/lambdaefectiva
  c <- Ls-Lq
  
  return(c(Ls,Lq,Ws,Wq,c))
}



###################
# Funciones M/M/c #
###################

MMc_p0 <- function (lambda,mu,c) {
  
  ro <- lambda/mu
  fi <- ro/c
  primparte <- 0
  
  for(n in c(0:(c-1))){
    primparte <- primparte + (ro^n)/factorial(n)} 
  
  p0 <- (primparte + (ro^c/factorial(c))*(1/(1-fi)))^(-1)
  return(p0)
}


MMc_medidas<- function(lambda,mu,c){
  
  ro <- lambda/mu
  fi <- ro/c
  n <- 0
  
  if(fi>=1){ 
    print(paste("El sistema no es estable: fi=",fi,sep=""))
    return(c(-99,-99,-99,-99,-99))
  }
  
  p0 <- MMc_p0(lambda,mu,c)
  
  Lq <- ( ro^(c+1)/((factorial(c-1)*(c-ro)^2)) ) *p0
  # Con las fórmulas de Little
  Ls <- Lq + (lambda/mu)
  c <- lambda/mu
  Wq <- Lq/lambda
  Ws <- Wq + (1/mu)
  
  return(c(Ls,Lq,Ws,Wq,c))
  
}


MMc_pn <- function (lambda,mu,c,n) {
  
  ro <- lambda/mu
  fi <- ro/c
  
  if(fi>=1){ 
    print(paste("El sistema no es estable: fi=",fi,sep=""))
    return(-99)
  }
   
  p0 <- MMc_p0(lambda,mu,c)
  
  if (n<c) {
    pn <- ((ro^n)/factorial(n))*p0
  } else {
    pn <- ((ro^n)/(factorial(c)*c^(n-c)))*p0
  }
  
  return(pn)
  
}



#################################
# Funciones M/M/c:DG/N/inf(c<N) #
#################################

MMcN_p0 <- function (lambda,mu,c,N) {
  
  ro <- lambda/mu
  fi <- ro/c
  
  if(c>=N){ 
    print("c no puede ser mayor o igual que N")
    return(-99)
  }
  
  primparte <- 0
  
  for(n in c(0:(c-1))){
    primparte <- primparte + (ro^n)/factorial(n)
  } 
  
  if(fi!=1){
    p0 <- (primparte + (((ro^c)*(1-((fi)^(N-c+1))))/(factorial(c)*(1-fi))))^(-1)
  } else {
    p0 <- (primparte + ((ro^c)/factorial(c))*(N-c+1))^(-1)  
  }

  return(p0)
  
}


MMcN_pn <- function (lambda,mu,c,n,N) {
  
  ro <- lambda/mu
  fi <- ro/c
  
  if(c>=N){ 
    print("c no puede ser mayor o igual que N")
    return(-99)
  }
  
  p0 <- MMcN_p0(lambda,mu,c,N)
  
  if (0<=n&&n<c){
    pn <- ((ro^n)/factorial(n))*p0
  } else {
    pn <- ((ro^n)/(factorial(c)*c^(n-c)))*p0
  }
  
  return(pn)
}


  
MMcN_medidas <- function(lambda,mu,c,N) {
  
  ro <- lambda/mu
  fi <- ro/c
  
  if(c>=N){ 
    print("c no puede ser mayor o igual que N")
    return(c(-99,-99,-99,-99,-99))
  }
  
  p0 <- MMcN_p0(lambda,mu,c,N)
  
  pN <- MMcN_pn(lambda,mu,c,N,N)
  
  lambdaperdida <- pN*lambda
  lambdaefectiva <- lambda-lambdaperdida
  
  if(c==ro){
    print(paste("fi es igual a ro, no se puede usar la fórmula de Lq"))
    return(c(-99,-99,-99,-99,-99))
  }
  
  Lq <- (((ro^(c+1))/(factorial(c-1)*(c-ro)^2))*(1-((fi)^(N-c+1))-(N-c+1)*(1-fi)*((fi)^(N-c))))*p0
  
  
  # Fórmulas de Little
  Wq <- Lq/lambdaefectiva
  Ws <- Wq + (1/mu)
  Ls <- lambdaefectiva*Ws
  c <- Ls-Lq
  
  return(c(Ls,Lq,Ws,Wq,c))
}







