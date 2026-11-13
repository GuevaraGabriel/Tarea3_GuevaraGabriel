# Recibe: un data frame de transacciones, ventana y k.alerta.
# Devuelve: un data frame con fecha, n.tx, media, desv y alerta.

alerta.movil <- function(df, ventana, k.alerta) {
  conteo.diario <- df |>
    count(fecha, name = "n.tx") |>
    arrange(fecha)
  
  
  # Crear columnas para guardar los resultados
  conteo.diario$media <- NA_real_
  conteo.diario$desv <- NA_real_
  conteo.diario$alerta <- NA
  
  # Calcular la media y desviación móvil
  for (i in (ventana + 1):nrow(conteo.diario)) {
    conteo.diario$media[i] <- mean(conteo.diario$n.tx[(i - ventana):(i - 1)])
    conteo.diario$desv[i] <- sd(conteo.diario$n.tx[(i - ventana):(i - 1)])
    conteo.diario$alerta[i] <-
      conteo.diario$n.tx[i] >
      conteo.diario$media[i] + k.alerta * conteo.diario$desv[i]
  }
  
  return(conteo.diario)
}

