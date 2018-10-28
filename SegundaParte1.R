library(igraph)

## https://www.r-bloggers.com/lexicographic-permutations-euler-problem-24/

nextPerm <- function(a) {
    #Encuentra el sufijo no creciente más largo.
  i <- length(a) 
  
  while (i > 1 && a[i - 1] >= a[i])
    i <- i - 1
  #i es el índice de cabeza del sufijo.
  #¿Estamos en la última permutación?.
  if (i <= 1) return(NULL)
  #a[i-1] es el pivote.
  #Encuentra el elmento más a la derecha  que excede el pivote.
  j <- length(a)
  while (a[j] <= a[i - 1]) 
    j <- j - 1
  #intercambiamos el pivote con a[j]
  aux <- a[i - 1]
  a[i - 1] <- a[j]
  a[j] <- aux
  #invierte el sufijo.
  a[i:length(a)] <- rev(a[i:length(a)])
  return(a)
}

##
#	construimos un grafo random con n nodos.
#	Usamos la libreria "igraph".
#	[1] https://www.r-graph-gallery.com/248-igraph-plotting-parameters/

buildGraph <- function(n) {
  adjm <- matrix(sample(0:1, n*n, replace=T, prob=c(0.45, 0.55)), nr=n) ## [1] ## 
  
  # hacemos simetrica (no dirigida) la matriz random.
  adjm <- (adjm + t(adjm))/2
  return(graph.adjacency(adjm, mode="undirected", diag=F))
}

n <- as.integer(readline(prompt = "Ingrese el orden del grafo: "))
  # construimos el grafo.
  g <- buildGraph(n)
  cycle <- c()	
    
    plot(g)
    break
    
  print("...")