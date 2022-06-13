datos = read.delim("C:/Users/rvozzi/OneDrive - genommalabinternacional/Documentos/ECD/DataMining/labo/exp/ST7610/exp_ST7610_cluster_de_bajas_6.txt", header = TRUE, sep = "\t", dec = ".")
datos$clase_ternaria<-NULL
datos$cluster2 = factor(datos$cluster2)
library(rpart)
library(rpart.plot)
mytree <- rpart(
  cluster2 ~., 
  data = datos, 
  method = "class",
  parms = list(split = 'information'), 
  minsplit = 2, 
  minbucket = 1,
  #cp = 0
)
prp(mytree, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
aggregate(x = datos[,3:158],
          by = list(datos$cluster2),
          FUN = mean)