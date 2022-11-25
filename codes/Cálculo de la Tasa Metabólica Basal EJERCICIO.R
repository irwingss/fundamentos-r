
peso=84
altura=190
edad=29

TMB = 10*peso + 6.26*altura-5*edad+5

TMB*1.2 #totalmente sedentario 0
TMB*1.375 #ligeramente activo (deportes 1-3dia/semana)
TMB*1.55 #moderadamente activo (3-5dia/semana)
TMB*1.725 #muy activo (6-7dia/semana)
TMB*1.912 #extremadamente activo, doble entrenamiento 



tmb <- function(peso, altura, edad, dias_entrenamiento){
  
  TMB = 10*peso + 6.26*altura-5*edad+5
  
  DE <- dias_entrenamiento
  multi = dplyr::case_when(
    DE == 0 ~ 1.2,
    DE %in% 1:2 ~ 1.375,
    DE  %in% 3:4 <=5 ~ 1.55,
    DE  %in% 5:6 ~ 1.725,
    DE  %in% 5:6 ~ 1.912
  )
  TMB*multi
}

tmb(peso = 83,altura = 190,edad = 29,dias_entrenamiento = 2)
# número de calorías que necesitas diarias para mantener tu peso
