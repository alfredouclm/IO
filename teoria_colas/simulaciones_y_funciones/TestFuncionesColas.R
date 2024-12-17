
#setwd
source('FuncionesColas.R')




lambda <- 3
mu <- 5

n <- 2 # Para probabilidad de que haya "n" clientes en el sistema o más
t <- 1 # Para probabilidad de esperar un tiempo "t"
N <- 3 # Clientes en los sistemas de cola limitados
c <- 2 # Número de servidores para sistemas M/M/c

decimales <- 3 # Número de decimales a mostrar en los resultados

###########################
# Pruebas con colas M/M/1 #
###########################

print("Pruebas M/M/1");

aux <- MM1_medidas(lambda,mu)
print(paste("      Ls: ",round(aux[1],decimales),sep=""))
print(paste("      Lq: ",round(aux[2],decimales),sep=""))
print(paste("      Ws: ",round(aux[3],decimales),sep=""))
print(paste("      Wq: ",round(aux[4],decimales),sep=""))
print(paste("       c: ",round(aux[5],decimales),sep=""))

print(paste("      pn: ",round(MM1_pn(lambda,mu,n),decimales),sep=""))
print(paste(" P(Ls>n): ",round(MM1_PLsMayorN(lambda,mu,n),decimales),sep=""))
print(paste(" P(Ws>t): ",round(MM1_PWsMayorT(lambda,mu,t),decimales),sep=""))
print(paste(" P(Wq>t): ",round(MM1_PWqMayorT(lambda,mu,t),decimales),sep=""))

print("");

####################################
# Pruebas con colas M/M/1:DG/N/inf #
####################################

print("Pruebas M/M/1:DG/N/inf");
aux <- MM1N_medidas(lambda,mu,N)
print(paste("      Ls: ",round(aux[1],decimales),sep=""))
print(paste("      Lq: ",round(aux[2],decimales),sep=""))
print(paste("      Ws: ",round(aux[3],decimales),sep=""))
print(paste("      Wq: ",round(aux[4],decimales),sep=""))
print(paste("       c: ",round(aux[5],decimales),sep=""))
print(paste("      pn: ",round(MM1N_pn(lambda,mu,N,n),decimales),sep=""))


print("");

###########################
# Pruebas con colas M/M/c #
###########################

print("Pruebas M/M/c");

aux <- MMc_medidas(lambda,mu,c)
print(paste("      Ls: ",round(aux[1],decimales),sep=""))
print(paste("      Lq: ",round(aux[2],decimales),sep=""))
print(paste("      Ws: ",round(aux[3],decimales),sep=""))
print(paste("      Wq: ",round(aux[4],decimales),sep=""))
print(paste("       c: ",round(aux[5],decimales),sep=""))
print(paste("      pn: ",round(MMc_pn(lambda,mu,c,n),decimales),sep=""))


print("");

#########################################
# Pruebas con colas M/M/c:DG/N/inf(c<N) #
#########################################

print("Pruebas M/M/c:DG/N/inf(c<N)");

aux <- MMcN_medidas(lambda,mu,c,N)
print(paste("      Ls: ",round(aux[1],decimales),sep=""))
print(paste("      Lq: ",round(aux[2],decimales),sep=""))
print(paste("      Ws: ",round(aux[3],decimales),sep=""))
print(paste("      Wq: ",round(aux[4],decimales),sep=""))
print(paste("       c: ",round(aux[5],decimales),sep=""))
print(paste("      pn: ",round(MMcN_pn(lambda,mu,c,n,N),decimales),sep=""))


  