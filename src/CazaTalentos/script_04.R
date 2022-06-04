require("data.table")

#set.seed( 102191 )

#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar  <- function( prob, qty )
{
  return( sum( runif(qty) < prob ) )
}

mejor <- 0.7
peloton   <-  ( 501:599 ) / 1000

tiros <- c()
cortes <- c()
m <- 1

for (k in 1:451)
{
  print(k)
  x <- c()
  for( i in 1:10000 )  #diez mil experimentos
  {
    x <- c(x,mapply( ftirar, mejor, k )) 
  }
  
  if(length(x[x<=m])/10000<0.01)
  {
    tiros <- c(tiros,k)
    cortes <- c(cortes,m)
    m <- m+1
  }
}

#hasta aca estoy bien
ganador <- c()
vtotal <- c()
for(d in 1:1000)
{
  mejor <- 0.7
  jugadores <-  c( mejor, peloton )
  total <- 0
  vaciertos_acum <- c()
  print(d)
  i<-1
  for (i in length(tiros))
  {
    #de aca para abajo hay que corregir
    #ver lo que tengo anotado
    if(i==1)
    {
      vaciertos  <- mapply( ftirar, jugadores, tiros[i])
      vaciertos_acum <- vaciertos
      tiros_ronda = length(jugadores)*tiros[i]
    }
    else
    {
      vaciertos  <- mapply( ftirar, jugadores, tiros[i]-tiros[i-1])
      vaciertos_acum <- vaciertos_acum + vaciertos
      tiros_ronda = length(jugadores)*(tiros[i]-tiros[i-1])
    }
    total <- total + tiros_ronda
    jugadores <- jugadores[which(vaciertos>cortes[i])] #elimino a los que hayan encestado 1 o menos tiros
    vaciertos_acum <- vaciertos_acum[which(vaciertos>cortes[i])]
  }
  indice <- which.max(vaciertos_acum)
  ganador <- c(ganador, jugadores[indice])
  vtotal <- c(vtotal, total)
}  