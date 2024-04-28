
require(palmerpenguins)
require(tidyverse)

datos <- penguins %>% select(flipper_length_mm, bill_length_mm)  %>% filter(!is.na(flipper_length_mm),!is.na(bill_length_mm))

Kamedias <- function(nxp, K.clus,  matrizCentroides,suma_distancias, clusters_viejos) {
  nxp <- as.matrix(nxp)
  filas <- c(1:nrow(nxp))
  
  if(missing(clusters_viejos)) {
    clusters_viejos <- NA
  }
 
  if (missing(matrizCentroides)){
    centroidesIds <- sample(filas, K.clus, replace = FALSE)
    matrizCentroides <- matrix(NA, nrow = K.clus, ncol = 2)
    for(i in c(1:K.clus)) {
      matrizCentroides[i, ] <- c(nxp[centroidesIds[i], ])
    }
  } 
  
  distsPorCentroide <- matrix(NA, nrow = K.clus, ncol = length(filas))
  
  
  #hacemos la matriz con las distancias de cada punto (hay n) y cada centroide (hay k.clus)
  for(i in 1:K.clus) {
    dists <- c()
    centroide_i <- matrizCentroides[i, ]
    for(f in filas) {
      dists <- append(dists, distancia(nxp[f,],centroide_i))
    }
    distsPorCentroide[i, ] <- dists
    
  }
  
  #creamos la matriz de las sumas de las distancias
  if(missing(suma_distancias)) {
    suma_distancias <- c()
  }
  suma_distancias <- append(suma_distancias, sum(distsPorCentroide))
  
  
  #asigno etiquetas
  clusters <- c()
  for(fila in filas) {
    clusters <- append(clusters, which.min(distsPorCentroide[ , fila]))
  }
  
  res <- list(clusters, matrizCentroides,suma_distancias)
  

  if(!is.na(clusters_viejos) && sum(clusters_viejos == clusters) == length(clusters)) {
    #grafico los clusters
    par(pty="s")
    plot(nxp, col=clusters+1, pch=16)
    points(matrizCentroides[,1],matrizCentroides[,2],pch=4, cex=2)
    
    return(res)
    
  } else{
    #paso de recalculo
    for(j in 1:K.clus){
      for(l in 1:ncol(nxp)){
        matrizCentroides[j,l] <- mean(nxp[clusters==j,l])
      }
    }
    
    
      
    return(Kamedias(nxp, K.clus, matrizCentroides,suma_distancias, clusters))
  }
  
}


distancia <- function(coords, centro) {
  sqrt(sum((coords-centro)^2))
}


matriz <- matrix(c(1,3,1,4,2,4,4,1,4,2,5,1,5,2), nrow = 7, ncol = 2, byrow = TRUE)
par(pty="s")

print(Kamedias(datos,5))




