require("data.table")

set.seed( 102191 )

#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar  <- function( prob, qty )
{
  return( sum( runif(qty) < prob ) )
}

mejor <- 0.7

x <- c()

for( i in 1:10000 )  #diez mil experimentos
{
  x <- c(x,mapply( ftirar, mejor, 6))
}

length(x[x<=2])/10000
  