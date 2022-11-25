# Clase 02
# Estructuras de datos: vectores

# ---------------------------------------------------------------------------- -
# EJERCICIO 1: Vectores y Data.Frame

# Prepara todo para el ejercicio
{
  # Creación de la base de datos
  conteos <- c(18,17,15,20,10,20,25,13,12)
  set.seed(123)
  abundancia <- rpois(9,10)
  set.seed(123)
  medicion <- rnorm(9)*sqrt(conteos)
  tratamiento <- gl(3,3)
  DF <- data.frame(tratamiento, abundancia,
                   medicion, conteos)

  # Modelo generalizado lineal
  mod2 <- glm(conteos ~ abundancia  +
                     medicion + tratamiento, data=DF,
                   family = "poisson")
  # Todo listo para el ejercicio
}

# Extraer los coeficientes del modelo llamado mod2
coef_poisson <- coef(mod2)
coef_poisson

# Cambiarles el nombre por intercepto al primer elemento
# y beta del 1 a 4

names(coef_poisson) <- c("Intercepto", paste0("Beta", 1:4))

# Revisando el cambio
coef_poisson

# Extraer la posición del Beta 2
coef_poisson[3]

# Exponenciar con la función exp() el coeficiente del Beta 2
exp(coef_poisson[3])
exp(coef_poisson[3]*10)

# Revisa el contenido de la base DF con la
# que se creo el modelo mod2
View(DF)
str(DF)

# Indexa las filas 4 y 8
DF[c(4,8),]

# Indexa las filas 4 y 2 y las columnas 2 y 4
DF[c(4,2),c(2,4)]

# Extrae con [] las columnas abundancia y medicion
# y guarda cada una como un objeto en el ambiente
names(DF)
ab1 <- DF[,"abundancia"]
ab2 <- DF[,2]

identical(ab1, ab2)

names(DF)
med1 <- DF[,"medicion"]
med2 <- DF[,3]

identical(med1, med2)

# Extrae con $ las columnas abundancia y medición
# y guarda cada una como un objeto en el ambiente
ab <- DF$abundancia
med <- DF$medicion


# Revisa la longitud del vector ab
length(ab)

# Crea un rango de valores que vaya
# de 1 hasta la longitud del vector ab
1:length(ab)
seq_along(ab)

# Asigna un ID a cada posición del vector ab
names(ab) <- paste0("Pos",seq_along(ab))
ab

# Si quieres rehacer el ejercicio, vuelve a ejecutar la
# sección inicial "Prepara todo para el ejercicio".

# ---------------------------------------------------------------------------- -
# EJERCICIO 2: Factores

# Prepara todo para el ejercicio
{
  # Crear la base de ejemplo
  Etiquetas <-  c("Europa","Asia","América","Oceania")
  set.seed(123)
  datos_fct <- sample(Etiquetas, size=140,
                      replace=TRUE, prob = c(0.4,0.3,0.1,0.2))
  Tabla <- fct_count(datos_fct)
  names(Tabla) <- c("Continente", "N")
  Tabla$Continente <- factor(Tabla$Continente)

  # Función barrasContinente() para graficar
  barrasContinente <- function(x) {
    instalar <- function(pqt){
      pqt <- as.character(pqt)
      if (!require(pqt, character.only = TRUE)){
        install.packages(pqt)
        library(pqt)
      }
    }
    paquetes <- c("tidyverse","ggthemes")
    for (i in seq_along(paquetes)){
      instalar(paquetes[i])
    }
    ggplot(x, aes(x=Continente, y=N, fill=Continente))+
      geom_bar(stat="identity")+
      scale_fill_tableau()+
      theme_minimal()
  }
  # Todo listo para el ejercicio
}

# Revisar los niveles de un factor
levels(Tabla$Continente)
barrasContinente(Tabla)

# Cambiar el nivel base (primer nivel) de un factor
F_reordenado <- relevel(Tabla$Continente, ref = "Oceania")
Tabla$Continente <- F_reordenado
barrasContinente(Tabla)

# Cambiar el orden de los niveles a mano
F_reordenado <- fct_relevel(Tabla$Continente,
                           "Asia","Europa","Oceania","America")
Tabla$Continente <- F_reordenado
barrasContinente(Tabla)

# Reordenar el factor en base a su frecuencia (menor a mayor)
F_reordenado <- fct_reorder(Tabla$Continente, .x = Tabla$N)
Tabla$Continente <- F_reordenado
barrasContinente(Tabla)

# Reordenar el factor en base a su frecuencia (mayor a menor)
F_reordenado <- fct_reorder(Tabla$Continente, .x = Tabla$N, .desc = TRUE)
Tabla$Continente <- F_reordenado
barrasContinente(Tabla)

# Cambiar los niveles que
# no son de interés por una etiqueta única
F_reordenado <- fct_other(Tabla$Continente,
                   keep = c("Europa","Asia"),
                   other_level = "Otros continentes")
Tabla$Continente <- F_reordenado
barrasContinente(Tabla)

# Cambiar el nombre de uno más niveles
F_reordenado <- fct_recode(Tabla$Continente,
                           "Europe" = "Europa",
                           "Others" = "Otros continentes")
Tabla$Continente <- F_reordenado
barrasContinente(Tabla)

# Si quieres rehacer el ejercicio, vuelve a ejecutar la
# sección inicial "Prepara todo para el ejercicio".





# ----------------------------------------------------
# Crea la función area de circulo
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

