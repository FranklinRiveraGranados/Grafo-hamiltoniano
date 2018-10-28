#Devuelve una permutación de un vector usando ordenamiento lexicográfico.
library(igraph)

## https://www.r-bloggers.com/lexicographic-permutations-euler-problem-24/

nextPerm <- function(a) {
  # Encuentra el sufijo no creciente más largo.
  i <- length(a) 
  
  while (i > 1 && a[i - 1] >= a[i])
    i <- i - 1
  # i es el índice de cabeza del sufijo.
  # ¿Estamos en la última permutación?
  if (i <= 1) return(NULL)
  #a[i-1] es el pivote.
  # Encuentra el elemento más a la derecha que excede el pivote.
  j <- length(a)
  while (a[j] <= a[i - 1]) 
    j <- j - 1
  #intercambiar pivote con j.
  aux <- a[i - 1]
  a[i - 1] <- a[j]
  a[j] <- aux
  #Invierte el sufijo.
  a[i:length(a)] <- rev(a[i:length(a)])
  return(a)
}