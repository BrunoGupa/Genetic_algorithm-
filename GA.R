# Este c??digo es la adaptaci??n en R de un algoritmo gen??tico dise??ado para Python. 
# La versi??n original puede encontrarse en:
#https://www.electricmonk.nl/log/2011/09/28/evolutionary-algorithm-evolving-hello-world/

#Descripci??n: 
#En este algoritmo se introduce una palabra de 13 caracteres y a trav??s de mutaciones
#se busca llegar a la palabra "Hello World!"
#Obs. con cualquier string 'source' que intent??, tarda de 2500 a 3500 iteraciones en 
#llegar al resultado deseado.


Sys.setlocale("LC_ALL", "en_US.UTF-8")
#paquete para escribir en espa??ol

source <- "1fth 4fAs_?[2"
target <- "Hello, World!"

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
  for (i in 1: (nchar(source))) {
    fitval <- fitval + (asc(substr(target,i,i)) - asc(substr(source,i,i))) ** 2
  }
  return(fitval)
}


#delimitamos la lista de los posibles caracteres imprimibles (ascci) en
#codificaci??n decimal.
numbers_character <- c(32:126)

mutate <- function(source){
  # Vamos a mutar un gen aleatorio de nuestro string source
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
  
  alelo <- sample(1:(nchar(source)), 1) #escogemos un entero aleatorio entre 1 y 13

  adn <- asc(substr(source,alelo,alelo)) + sample(c(-1,0,1),1, replace = TRUE) 
  #con 'substr()' seleccionamos al gen que se encuetra en el alelo seleccionado. 
  #con 'asc()' lo convertimos en un numero entero.
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
  substring(source, alelo) <- c(gen)
  mutation <-source
  
  return(mutation)
}


# procedemos a juntar todo. Se muta nuestro source orignial y 
# se procede a valuar de manera iterativa hasta que la funci??n 
# fitnes nos de cero. 

fitval <- fitness(source, target)
fitval

i = 1
while(TRUE){
  i <- i + 1
  
  m = mutate(source)
  fitval_m = fitness(m, target)
  if( fitval_m < fitval){ # Si el fitval nuevo es mejor que el anterior, reemplazamos. De lo contrario nos quedamos con el anterior. 
    fitval <- fitval_m
    source <- m
    print(c(i, fitval_m, m))
  } 
  if(fitval == 0){#cuando el valor fitval da cero, alcanzamos nuestro objetivo.
    break
  }
}

