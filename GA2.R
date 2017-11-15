
# En esta ocasi??n vamos a generar una poblaci??n aleatoria. 
# La poblaci??n estar?? compuesta de cromosomas cuyos caracteres 
# se obtienen de manera aleatoria. 
# 
# Se aplicar?? el m??todo usual de los Algoritmos gen??ticos.

Sys.setlocale("LC_ALL", "en_US.UTF-8")
#paquete para escribir en espa??o

target <- "Hello, World!"

GENSIZE <- 20

# copiamos las funci??nes 'asc', 'chr' y 'fitness' del algoritmo anterior

asc <- function(x) { strtoi(charToRaw(x),16L) }
#Creamos una fuci??n que cambia los caracteres imprimibles (ascci) a elemenotos en
#base decimal.

chr <- function(n) { rawToChar(as.raw(n)) }
#Creamos la funci??n inversa de 'asc'.

fitness <- function(source, target){
  # La funci??n fitness mide la distacia que hay entre el string 
  # sourse y target. Para hacer esto, se compara uno a uno los 
  # caracteres a trav??s de la funci??n utf8ToInt() la cual arroja un 
  # entero ??nico para cada caracter.
  # 
  # Para hacer esta comparaci??n se hace la diferencia entre ambos 
  # caracteres y se eleva al cuadrado. La raz??n de elevar al
  # cuadrado es para obligar que la funci??n fitness sea
  # cero ??nicamente cuando el source y el target sean iguales.
  # 
  # De otro modo, la suma de n??meros negativos y positivos 
  # podrian dar cero aun cuando no todos ellos sean iguales 
  # a cero. 
  # 
  # El objetivo es lograr que la finci??n fitness sea cero. 
  
  fitval <- 0
  for (i in 1: nchar(target)) {
    fitval <- fitval + (asc(substr(target,i,i)) - asc(substr(source,i,i))) ** 2
  }
  return(fitval)
}

#delimitamos la lista de los posibles caracteres imprimibles (ascci) en
#codificaci??n decimal.
numbers_character <- c(32:126)


#Generamos la poblaci??n aleatoria
population <- vector("list", GENSIZE)  #Creamos una lista en donde se ir??n poniendo los nuevos individuos junto con su funci??n fitness.
population
i <- 1
for(i in 1:GENSIZE){
  
  cromo <- vector("expression", nchar(target))
  x <- 1
  for(x in 1:(nchar(target))){
    random_gen <- chr(sample(32:126,1)) #se escoje cada gen alatoriamente de la lista de caracteres imprimibles 'numbers_character'.
    cromo[x] <- c(random_gen) #se va agregando cada gen al cromosoma correspondiente.
    x <- x + 1
  }
  
  cromo <-paste(cromo,collapse="") #convertimos al vector 'cromo' en un string (una sola palabra).
  valor <- fitness(cromo, target) #Se califica cada cromosoma con la funci??n fitness  
  
  population[[i]] <- list(cromo, valor) #Se agrega a la poblaci??n cada nuevo cromosoma con su respectiva calificaci??n fitness.
  i <- i + 1
}



# De acuerdo con esta fuente, se procede a seleccionar al azar dos 	
# padres para crear un desendiente. 
# Es evidente que la mejor forma de hacer la selecci??n es tomar
# a los padres  dentro de los mejores candidatos. 
# Para hacer  esto, vamos a  generar dos n??meros aleatorios entre 0 y 1 
# y multiplicarlos entre s??. Despu??s multiplicaremos este n??mero
# por 20. 
# 
# La parte entera de ese numero aleatorio estar?? entre 0 y 20 pero 
# tendr?? una mayor probabilidad de ser un entero peque??o.
# 
# Los padres ser??n los cromosomas que ocupan el lugar de este n??mero
# aleatorio (haciendo dos veces el procedimiento)



random_padre <- function(population){
  #creamos el numero aleatorio, dandole mayor probabilidad de salir a los n??meros peque??os.
  aleatorio =  runif(1) * runif(1) * (GENSIZE - 1)
  aleatorio = 1 + floor(aleatorio) #tomamos al mayor entero menor o igual que 'aleatorio'.
  
return(population[[aleatorio]][[1]])
}


#Definimos la funci??n cruza, la cual combinar?? los genes de los padres seleccionados. 
#se generan 13 n??meros aleatorios entre 1 y 2, que corresponden a tomar 
#el i-??simo gen del padre1 o del padre2. 
cruza <- function(padre1, padre2){
  j <- 1
  for(j in 1: nchar(target)){
    xy <- genes_padres[[j]][[sample(1:2,1)]]
    hijo_cromo[[j]] <- xy
    j = j + 1
  }
  hijo_cromo <- paste(hijo_cromo,collapse="")
  return(hijo_cromo)
}


#Copiamos del anterior codigo la funci??n mutaci??n y hacemos ligeros cambios
mutate <- function(mut){
  # Vamos a mutar un gen aleatorio de nuestro string hijo_cromo
  # cambianod en +1, 0 o -1 su valor. 
  # 
  # Elegimos de manera aleatoria la posici??n del gen (alelo).
  # 
  # Elegimos el elemento del string que esta en la posici??n
  # del alelo seleccionado aleatoriamente.
  # 
  # La funci??n chr() es la inversa de asc(), por lo que nos 
  # regresar?? un caracter despu??s de mutar con +1 o -1
  # el numero ??nico con el que se representa el caracter original.
  
  
  
  alelo <- sample(1:nchar(target), 1) #escogemos un entero aleatorio entre 1 y 13
  
  adn <- asc(substr(mut,alelo,alelo)) + sample(c(-1,0,1),1, replace = TRUE) 
  #con 'substr()' seleccionamos al gen que se encuetra en el alelo seleccionado. 
  #con 'asc()' lo convertimos en un numero entero ??nico para cada caracer.
  #despu??s se le suma un numero aleatorio (-1,0,1) generado por 'sample()'.
  #as?? pues 'adn' es un n??mero entero. 
  
  
  #Si 'adn' est?? en el conjunto 'numbers_chararter' se queda como est??.
  #Si 'adn<32' lo cambiamos a 126
  # y si 'adn > 126' lo cambiamos a 32.
  
  # Estas operaciones son con el fin de que al cambiar el c??digo ascii de
  # alg??n caracter, nos vuelva a dar el codigo de un caracter imprimible.
  if( (adn %in% numbers_character) == TRUE){
    adn <- adn
  } else {
    if(adn < 32 ){
      adn <-126
    } else {
      adn <- 32
    }
  }
  #convertimos al n??mero 'adn' en un gen o caracter imprimible
  gen <- chr(adn)
  
  #sustituimos al nuevo gen en su posici??n correspondiente (alelo)
  substring(mut, alelo) <- c(gen)
  mutation <-mut
  return(mutation)
}


# Lo que haremos ahora es un ciclo while donde
# 1. Ordenamos la poblaci??n de acuerdo a su funci??n fitness.
# 2. Si el mejor elemento (primer individuo de la lista) tiene fitness == o
# se termina el ciclo, en caso contrario continua otra iteraci??n.
# 3. seleccionamos a los padres
# 4. cruzamos
# 5. mutamos
# 6. evaluamos al nuevo individuo
# 7. Lo insertamos a la poblaci??n s??lo si es mejor que alguien de la poblaci??n. 
# Se desecha al individuo peor valuado, para evitar crecimiento desmedido de la poblaci??n.

l <- 1
while(TRUE){
  #1. Ordenamos la poblaci??n de acuerdo a su funci??n fitness.
  population <- population[order(sapply(population,'[[',2))]
  
  #imprimimos al mejor individuo de la lista
  best <- paste(population[[1]], collapse=', ' ) #colapsamos la lista a un solo renglón.
  print(c(l, best))
  
  l <- l +1
  # 2. Si el mejor elemento (primer individuo de la lista) tiene fitness == o
  # se termina el ciclo, en caso contrario continua otra iteraci??n.
  if(population[[1]][[2]] == 0){
    break
  }
  
  #3. seleccionamos a los padres
  padre1 <- random_padre(population) #seleccionamos al padre1 de forma aleatoria dentro de la poblaci??n
  padre2 <- random_padre(population) #an??logo a padre1

  padre1 <- as.list(strsplit(padre1, "")[[1]]) #hacemos una lista del string padre1
  padre2 <- as.list(strsplit(padre2, "")[[1]])# an??logo a padre1

  genes_padres <- mapply(c, padre1, padre2, SIMPLIFY=FALSE) #juntamos las listas de los padres en una s??la lista
  #de este modo 'genes_padres[[j]][[i]]' nos arroja el j-??simo caracter del padre i= 1,2.  

  # 4. cruzamos
  hijo_cromo <- vector("expression", nchar(target))  #Creamos una lista en donde se ir??n poniendo los genes del 
  #nuevo individuo generados por la funci??n 'cruza'.
  mut <- cruza(padre1, padre2) #Aplicamos la funci??n cruza para poder mutar al nuevo cromosoma
  
  # 5. mutamos
  new_cromo <- mutate(mut)
  
  # 6. evaluamos al nuevo individuo
  new_fitness <- fitness(new_cromo, target)
  
  hijo_cromo <- list(new_cromo, new_fitness)
  # 7. Insertamos el nuevo individuo a la poblaci??n s??lo si es mejor que alguien de la poblaci??n. 
  # Se desecha al individuo peor valuado, para evitar crecimiento desmedido de la poblaci??n.
  if(hijo_cromo[[2]] < population[[20]][[2]]){
    population[[20]] <- hijo_cromo
  }
  
}



