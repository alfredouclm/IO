setwd("D:/SynologyDrive/UCLM ADE/Investigacion Operativa (22-23)/Tema 3 - TeorÌa de Colas/practica4_juegos_y_colas/")

# setwd("~/SynologyDrive/UCLM ADE/Investigacion Operativa (22-23)/Tema 3 - TeoriÃÅa de Colas/practica4_juegos_y_colas")
source("FuncionesJuegos.R")
source("FuncionesColas.R")






m1 <- matrix(c(4,1,0,2),nrow=2,byrow=TRUE)
m2 <- matrix(c(2,1,0,2),nrow=2,byrow=TRUE)
e1 <- c("H","M")
e2 <- c("H","M")
j<-list(utilidad_J1=m1,utilidad_J2=m2,estrategias_J1=e1,estrategias_J2=e2)

muestra_juego(j) 
enash_ep(j)
enash_em(j)
aaaaaaa
jeed <- quita_eed(j,FALSE)
muestra_juego(jeed) 







m1 <- matrix(c(10,20,0,15),nrow=2,byrow=TRUE)
m2 <- matrix(c(10,0,20,15),nrow=2,byrow=TRUE)
e1 <- c("H","M")
e2 <- c("H","M")
j<-list(utilidad_J1=m1,utilidad_J2=m2,estrategias_J1=e1,estrategias_J2=e2)

muestra_juego(j) 
jeed <- quita_eed(j,FALSE)
muestra_juego(jeed) 



m1 <- matrix(c(10,20,0,15),nrow=2,byrow=TRUE)
m2 <- matrix(c(10,0,20,15),nrow=2,byrow=TRUE)
e1 <- c("H","M")
e2 <- c("H","M")
j<-list(utilidad_J1=m1,utilidad_J2=m2,estrategias_J1=e1,estrategias_J2=e2)




lambda <- 4
mu <- 6
N <- 5
n <- 5
p5 <- MM1N_pn(lambda,mu,N,n) # Probabilidad del sistema lleno
lambda.perdido <- p5*lambda
lambda.ef <- 4-lambda.perdido

p0 <- MM1N_pn(lambda,mu,N,0)

aux <- MM1N_medidas(lambda,mu,N)
aux[4]

5-aux[1]


for(clientes in 1:15){
  # Probabilidad del sistema lleno
  plleno <- MM1N_pn(lambda,mu,clientes,clientes) 
  porc <- round(plleno*100,2)
  print(paste("Con ",clientes-1," plazas probabilidad del ",porc,'%',sep=""))
}






m1 <- matrix(c(0,1,2,6,7,1,4,5,0),nrow=3,byrow=TRUE)
m2 <- matrix(c(0,0,1,3,-1,0,5,2,3),nrow=3,byrow=TRUE)
e1 <- c("A","B","C")
e2 <- c("X","Y","Z")
j<-list(utilidad_J1=m1,utilidad_J2=m2,estrategias_J1=e1,estrategias_J2=e2)
jd <- quita_eed(j,F)
muestra_juego(jd)


enash_ep(jd)
h <- enash_em(jd)


aux <- MMc_medidas(17.5,10,2)
aux*60
aux

MMc_medidas(17.5,10,2)[1]



