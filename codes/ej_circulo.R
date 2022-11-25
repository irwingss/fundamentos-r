# Crea la función
areaCirculo <- function(radio){
  area <- pi*radio^2
  area_r <- round(area)
  if(area_r %% 2 == 0) {
    return(area)
  } else {
    "El área no es par, so sad"
  }
}

areaCirculo(10)

for (i in 1:30) {
  if(is.numeric(areaCirculo(i))) {
    cat("r = ", i, "; área =",areaCirculo(i),"\n")
    } else {
      return(i)
      }
}



a2 <- Vectorize(areaCirculo)
a2(1:30)
