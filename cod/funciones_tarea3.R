#Ejercicio 1

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

#===============================================================================

#Ejercicio 2

# Recibe: un vector numerico de montos.
# Devuelve: la desviacion absoluta media (MAD) de Benford.

benford.mad <- function(x) {
  # Conservar montos positivos y no faltantes
  x <- x[!is.na(x) & x > 0]
  
  # Obtener el primer digito de cada monto
  digito <- floor(x / 10^floor(log10(x)))
  
  # Contar las frecuencias de los digitos del 1 al 9
  frecuencia <- tabulate(digito, nbins = 9)
  
  # Calcular las proporciones observadas
  p.obs <- frecuencia / sum(frecuencia)
  
  # Calcular las proporciones teoricas de Benford
  p.benford <- log10(1 + 1 / (1:9))
  
  # Calcular la desviacion absoluta media
  mad <- mean(abs(p.obs - p.benford))
  
  return(mad)
}

# Recibe: transacciones, canal y tabla de cantones.
# Devuelve: MAD de Benford para cada canton del canal.

benford.canton <- function(df, canal, cantones) {
  # Guardar el canal seleccionado
  canal.seleccionado <- canal
  
  # Filtrar las transacciones del canal
  datos.canal <- df |>
    filter(canal == canal.seleccionado)
  
  # Calcular el MAD de cada canton
  resultado <- datos.canal |>
    group_by(cod_canton) |>
    summarise(mad = benford.mad(monto_crc), .groups = "drop")
  
  # Agregar los nombres de los cantones
  resultado <- resultado |>
    left_join(cantones, by = "cod_canton") |>
    arrange(desc(mad))
  
  return(resultado)
}










