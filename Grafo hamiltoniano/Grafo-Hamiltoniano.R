library(igraph)


## https://www.r-bloggers.com/lexicographic-permutations-euler-problem-24/

nextPerm <- function(a) {
  
  i <- length(a) 
  
  while (i > 1 && a[i - 1] >= a[i])
    i <- i - 1
  
  if (i <= 1) return(NULL)
  
  j <- length(a)
  while (a[j] <= a[i - 1]) 
    j <- j - 1
  
  temp <- a[i - 1]
  a[i - 1] <- a[j]
  a[j] <- temp
  
  a[i:length(a)] <- rev(a[i:length(a)])
  return(a)
}


##
#	Resuelve si un grafo es hamiltoniano.
#	input : matriz de adjacencia en igraph.
#	output : TRUE o FALSE si es hamiltoniano 
#	o no respectivamente.
#
#	[1] http://igraph.org/r/doc/
#	[2] https://stat.ethz.ch/R-manual/R-devel/library/base/html/bitwise.html 
#	[3] https://stackoverflow.com/questions/4678333/n-n-1-what-does-this-expression-do
#

isHamiltonian <- function(g, cycle) {		
  
  n <- vcount(g)
  
  p <- 1:n
  
  times = 1
  for (i in 1:n)
    times <= times *  i
  
  hamiltonian <- T
  while(times > 0) {
    times <- times - 1
    hamiltonian <- T
    for (i in 1:(n-1)) {
      if (!are_adjacent(g, p[i], p[i+1])) {
        hamiltonian <- F
      }
    }
    
    if (!are_adjacent(g, p[n], p[1])) {
      hamiltonian <- F
    }
    
    if (hamiltonian == T) break
    p <- nextPerm(p) 
  }
  
  if (hamiltonian == F) return (F)
  eval.parent(substitute(cycle <- c(p, p[1])))
  return(T)
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
while (TRUE) {
  # construimos el grafo hasta que sea hamiltoniano.
  g <- buildGraph(n)
  cycle <- c()	
  
  
  if (isHamiltonian(g, cycle)) {
    # visualiza los nodos tan grandes como su grado. 
    # http://www.shizukalab.com/toolkits/sna/plotting-networks-pt-2
    #V(g)$size=degree(g)*5
    
    #dado el ciclo formamos las aristas.
    edgeCycle <- c()
    for (i in 1:(length(cycle)-1)) {	
      edgeCycle <- c(edgeCycle, c(cycle[i], cycle[i+1]))
    }
    
    #cambiando de color al ciclo.
    E(g)$color <- 'green'    #color que no forma parte del ciclo hamiltoniano.
    E(g)$color[get.edge.ids(g, edgeCycle)] <- 'blue'			#color del ciclo hamiltoniano
    
    plot(g)
    break
  }
  print("...")
}
