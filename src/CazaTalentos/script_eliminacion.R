require("data.table")

vaciertos_acum = c(242, 187, 170, 199, 207, 198,
                   195, 201, 205, 199, 198, 202, 209, 204)
jugadores = c(0.700, 0.524, 0.531, 0.559, 0.575,
              0.581, 0.583, 0.587, 0.589, 0.592, 
              0.593, 0.597, 0.598, 0.599)

#quiero eliminar los h peores
h <- 2

eliminados <- head(sort(vaciertos_acum),h)
jugadores <- jugadores[-which(vaciertos_acum %in% eliminados)]
vaciertos_acum <- vaciertos_acum[-which(vaciertos_acum %in% eliminados)]
print(eliminados)