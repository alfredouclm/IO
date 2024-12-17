

setwd("D:/SynologyDrive/UCLM ADE/Investigacion Operativa (22-23)/Tema 3 - Teoría de Colas/practica3/")



# lambda <- 2 # clientes minuto
# 
# tiempo <- 180 # 3 horas
#   
# d <- rexp(2000,lambda)
# acc <- 0
# llegadas <- 0
# for(i in 1:length(d)){
#   
#   if((acc+d[i])<tiempo){
#     acc <- acc+d[i]
#     llegadas <- i
#   }
#   
# }
# 
# print(dpois(9,3*2))
# 
# aaa


lambda <- 2 # llegadas por ud tiempo
tiempo_total <- 8 # minutos
numero_llegadas <- 9

a <- c()
for(i in 1:1000){
  
  cnt <- 0
  ttot <- 0
  while(ttot < tiempo_total){
    
    t <- rexp(1,lambda)
    ttot <- ttot + t
    if(ttot<tiempo_total){
      cnt <- cnt + 1
    }
  }
  a <- c(a,cnt)
}

print(paste0("Simulado: ",length(which(a==numero_llegadas))/length(a)))
print(paste0("Analítico: ",dpois(numero_llegadas,tiempo_total*lambda)))
