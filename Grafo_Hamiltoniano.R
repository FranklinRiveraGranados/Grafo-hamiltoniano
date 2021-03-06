library(igraph)


## https://www.r-bloggers.com/lexicographic-permutations-euler-problem-24/

nextPerm <- function(a) {
  #Encuentra el sufijo no creciente m�s largo.
  i <- length(a) 
  
  while (i > 1 && a[i - 1] >= a[i])
    i <- i - 1
  #i es el �ndice de cabeza del sufijo.
  #�Estamos en la �ltima permutaci�n?
  if (i <= 1) return(NULL)
  #a[i-1] es el pivote.
  #Encuentra el elemento m�s a la derecha que excede el pivote.
  j <- length(a)
  while (a[j] <= a[i - 1]) 
    j <- j - 1
  #intercambiamos el pivote con a[j].
  aux <- a[i - 1]
  a[i - 1] <- a[j]
  a[j] <- aux
  #Invierte el sufijo.
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

############################################################################################
#########   ####################   ################   ##############   #####################

#Esta funci�n verifica si el grafo ingresado en hamiltoniano.
esHamiltoniano <- function(g, cycle) {		
  
  n <- vcount(g)  # N�mero de v�rtices del grafo
  
  p <- 1:n   #p: [1] 1 2 3 4 ... n
  
  cont = 1
  for (i in 1:n)
    cont <= cont *  i    ###calculamos cont = n! = 1*2*3*4*5*...*n, lo usaremos para verificar las n! permutaciones posibles
  
  hamiltoniano <- T    ###inicializamos con TRUE
  
  while(cont > 0) {   
    cont <- cont - 1 
    for (i in 1:(n-1)) { ### Verificamos si los elementos de las posicisiones desde 1 hasta (n-1) est�n unidos por un camino(ciclo).
      if (!are_adjacent(g, p[i], p[i+1])) {   ### Si los v�rtices p[i] y p[i+1] no son adyacentes(es decir no hay arista que los una) entra al if
        hamiltoniano <- F                     ### y hamiltoniano es FALSE
      }
    }
    
    ### Verificamos si el posible camino encontrado en el paso anterior es un ciclo.
    if (!are_adjacent(g, p[n], p[1])) { ### Si los v�rtices p[n] y p[1] no son adyacentes(es decir no hay arista que los una) entra al if
      hamiltoniano <- F                 ### y hamiltoniano es FALSE
    }
    
    if (hamiltoniano == T) break  ### Si hamiltoniano es TRUE entonces sale del while
    p <- nextPerm(p)              ### para cada valor de "cont" hay una permutaci�n diferente para los elementos de p.
  }
  
  if (hamiltoniano == F) return (F)
  
  eval.parent(substitute(cycle <- c(p, p[1])))
  
  return(T)
}

###
##	construimos un grafo random con n nodos.
##	Usamos la libreria "igraph".
##	[1] https://www.r-graph-gallery.com/248-igraph-plotting-parameters/

buildGraph <- function(n) {
  
  ####
  ### matrix: crea una matriz a partir del conjunto de valores dados.
  ### Usamos la funci�n "sample" para generar aleatoriamente n�mero enteros en el intervalo 0 y 1.
  ## 0:1 = significa que el intervalo de datos va a estar comprendido entre ambos n�meros.
  ## n*n = significa la cantidad de n�meros aleatorios que quieres salgan del intervalo anterior.
  ## replace= T = para que puedan repetirse los n�meros aleatorios (es decir, obtener por ejemplo dos n�meros 1).
  ## prob: Un vector de pesos de probabilidad para obtener los elementos del vector que se muestrea.
  ## nr: n�mero de filas.
  ####
  adjm <- matrix(sample(0:1, n*n, replace=T, prob=c(0.45, 0.55)), nr=n) ## [1] ## 
  
  ### hacemos simetrica (no dirigida) la matriz random.
  adjm <- (adjm + t(adjm))/2
  
  ####
  ### graph.adjacency : crea el grafo a partir de matrices de adyacencia.
  ## adjm : matriz de adyacencia.
  ## "undirected": para que el grafo sea no dirigido.
  ## F : para que la diagonal sea 0 y asi evitar que se formen lazos en el grafo.
  ####
  return(graph.adjacency(adjm, mode="undirected", diag=F)) #
}

n <- as.integer(readline(prompt = "Ingrese el orden del grafo: "))  ### n: n�mero de v�rtices de nuestro grafo.
while (TRUE) {
  
  ### construimos el grafo hasta que sea hamiltoniano.
  g <- buildGraph(n)
  cycle <- c()	
  
  if (esHamiltoniano(g, cycle)) {  #Entra al if cuando encuentra el grafo es hamiltoniano
    ### visualiza los nodos tan grandes como su grado. 
    ### http://www.shizukalab.com/toolkits/sna/plotting-networks-pt-2
    
    ### dado el ciclo formamos las aristas.
    edgeCycle <- c()   ###borde del ciclo
    for (i in 1:(length(cycle)-1)) {	
      edgeCycle <- c(edgeCycle, c(cycle[i], cycle[i+1]))
    }
    
    ### cambiando de color al ciclo.
    E(g)$color <- 'green'    #color que no forma parte del ciclo hamiltoniano.
    E(g)$color[get.edge.ids(g, edgeCycle)] <- 'blue'			#color del ciclo hamiltoniano
    
    plot(g)  ###representaci�n gr�fica del grafo
    title(main = "GRAFO HAMILTONIANO") #Le a�ade t�tulo 
    break 
  }
  #print("...")
}

####################################################################################
####################   ####################   ############   #######################