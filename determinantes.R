# Determinante de una matriz 2x2

det_2x2 <- function(x){
  if (! is.matrix(x)){
    stop("Debe ingresa una matriz.")
  }else if (dim(x)[1] != dim(x)[2]){
    stop("Debe ingresar una matriz cuadrada.")
  }else if (dim(x)[1] != 2 && dim(x)[2] != 2){
    stop('Debe ingresar una matriz de 2x2.')
  }else{
    cuenta <- (x[1,1] * x[2,2]) -(x[2,1] * x[1,2]) 
    return(cuenta)
  }
}

# Determinante de una matriz 3x3

det_3x3 <- function(x){
  if(!is.matrix(x)){
    stop("Debe ingresar una matriz.")
  }else if (dim(x)[1] != dim(x)[2]){
    stop("Debe ingresar un matriz cuadrada.")
  }else if (dim(x)[1] != 3 && dim(x)[2] != 3){
    stop("Debe ingresar un matriz de 3x3.")
  }else{
    cuenta2 <- ((x[1,1]*x[2,2]*x[3,3])+(x[1,2]*x[2,3]*x[3,1])+(x[2,1]*x[3,2]*x[1,3]))-((x[1,3]*x[2,2]*x[3,1])+(x[2,1]*x[1,2]*x[3,3])+(x[1,1]*x[2,3]*x[3,2]))
    return(cuenta2)
  }
}

