require("data.table")

set.seed( 102191 )

#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar  <- function( prob, qty )
{
  return( sum( runif(qty) < prob ) )
}

mejor <- 0.7

tiros <- c(6)
cortes <- c(1)

for (k in 1:50)
{
  x <- c()
  print(k)
  for( i in 1:10000 )  #diez mil experimentos
  {
    x <- c(x,mapply( ftirar, mejor, 6+k )) 
  }
  
  if(length(x[x<=tail(cortes,n=1)+1])/10000<0.012)
  {
    tiros <- c(tiros,6+k)
    cortes <- c(cortes,tail(cortes,n=1)+1)
  }
}