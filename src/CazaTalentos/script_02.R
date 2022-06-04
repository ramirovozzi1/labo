require("data.table")

#set.seed( 102191 )

#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar  <- function( prob, qty )
{
  return( sum( runif(qty) < prob ) )
}

mejor <- 0.7
peloton   <-  ( 501:599 ) / 1000
jugadores <-  c( mejor, peloton )

#voy a calcular como voy a hacer los cortes para ir descalificando
#al principio los que hagan 6 tiros y metan 1 o menos quedan afuera
#el tamaño del corte me va a permitir controlar a que porcentaje
#voy dejando afuera
#a traves de la probabilidad puedo controlar que corte uso a medida
#que aumento el numero de tiros
tiros <- c(6)
cortes <- c(1)

for (k in 1:50)
{
  x <- c()
  for( i in 1:10000 )  #diez mil experimentos
  {
    x <- c(x,mapply( ftirar, mejor, 6+k )) 
  }
  
  if(length(x[x<=tail(cortes,n=1)+1])/10000<0.006)
  {
    tiros <- c(tiros,6+k)
    cortes <- c(cortes,tail(cortes,n=1)+1)
  }
}

#defino cuantos voy a eliminar (h)
h <- 3
#a partir de la ronda r
r <- 7

#realizo el experimento
total <- 0
vaciertos_acum <- c()

ganador <- c()
vtotal <- c()
for(m in 1:1000)
{
  mejor <- 0.7
  peloton   <-  ( 501:599 ) / 1000
  jugadores <-  c( mejor, peloton )
  total <- 0
  vaciertos_acum <- c()
  print(m)
  i<-1
  for (i in 1:12)
  {
    vaciertos  <- mapply( ftirar, jugadores, tiros[i])
    vaciertos_acum <- if(i==1){vaciertos} else {vaciertos_acum + vaciertos}
    tiros_ronda = length(jugadores)*tiros[i]
    total <- total + tiros_ronda
    jugadores <- jugadores[which(vaciertos>cortes[i])] #elimino a los que hayan encestado 1 o menos tiros
    vaciertos_acum <- vaciertos_acum[which(vaciertos>cortes[i])]
    if(i>=r && length(jugadores)>h+1)
      {
      eliminados <- head(sort(vaciertos_acum),h)
      jugadores <- jugadores[-which(vaciertos_acum %in% eliminados)]
      vaciertos_acum <- vaciertos_acum[-which(vaciertos_acum %in% eliminados)]
      }
    
  }

  #con los tiros que me sobran sigo tirando hasta llegar a 14000 tiros totales
  
  restantes = floor((15000-total)/length(jugadores))
  if(restantes>0)
  {
    vaciertos  <- mapply( ftirar, jugadores, restantes)
    vaciertos_acum <- vaciertos_acum + vaciertos
    tiros_ronda = length(jugadores)*restantes
    total <- total + tiros_ronda
  }
  mejor <- which.max(vaciertos_acum)
   
  ganador <- c(ganador, jugadores[mejor])
  vtotal <- c(vtotal, total)
  print(ganador)
  print(vtotal)
  print(length(jugadores))
}
print(length(ganador[ganador==0.7]))
print(length(vtotal[vtotal>15000]))