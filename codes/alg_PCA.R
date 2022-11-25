alg_PCA <- function(base) {
  # Librerías
  require(tidyverse)
  require(factoextra)

  # Primer if else: testeo base numérica
  if (all(apply(base, 2, is.numeric))) {
    base1 <- base
  } else if (!all(apply(base, 2, is.numeric))) {
    base1 <- base %>% select_if(is.numeric)
  }

  # Decisión del investigador para estandarizar
  desvest <- apply(base1, 2, sd)
  print(desvest)

  respuesta <- readline(prompt = "Ingresa 1 para estandarizar la base, 2 para no estandarizar: ")

  # Segundo if else: estandarización del PCA
  # basado en respuesta del investigador
  if (respuesta == 1) {
    PCA <- prcomp(base1, scale = TRUE)
    print(PCA)
    print(summary(PCA))
    print(fviz_pca_biplot(PCA))

  } else if (respuesta == 2) {
    PCA <- prcomp(base1, scale = FALSE)
    print(PCA)
    print(summary(PCA))
    print(fviz_pca_biplot(PCA))
  }
}
