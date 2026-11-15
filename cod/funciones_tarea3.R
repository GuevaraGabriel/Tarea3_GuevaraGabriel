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
# Devuelve: cod_canton, canton, n, mad y clase,
# ordenados de mayor a menor MAD.

benford.canton <- function(df, canal, cantones) {
  canal.seleccionado <- canal
  
  resultado <- df |>
    filter(canal == canal.seleccionado, !is.na(monto_crc), monto_crc > 0) |>
    group_by(cod_canton) |>
    summarise(n = n(),
              mad = benford.mad(monto_crc),
              .groups = "drop") |>
    left_join(cantones, by = "cod_canton") |>
    mutate(
      clase = case_when(
        mad <= 0.006 ~ "Conformidad cercana",
        mad <= 0.012 ~ "Conformidad aceptable",
        mad <= 0.015 ~ "Conformidad marginal",
        TRUE ~ "No conformidad"
      )
    ) |>
    select(cod_canton, canton, n, mad, clase) |>
    arrange(desc(mad))
  
  return(resultado)
}

#===============================================================================

#Ejercicio 3

# Recibe: transacciones, tasa efectiva anual y canal.
# Devuelve: lista con tabla mensual, tasa mensual,
# valor acumulado e intereses ganados.

valor.acumulado <- function(df, tasa.anual, canal) {
  # Calcular la tasa efectiva mensual
  tasa.mensual <- (1 + tasa.anual)^(1 / 12) - 1
  
  # Guardar el canal seleccionado
  canal.seleccionado <- canal
  
  # Sumar los montos de cada mes
  montos.mensuales <- df |>
    filter(canal == canal.seleccionado) |>
    mutate(mes = as.integer(format(fecha, "%m"))) |>
    group_by(mes) |>
    summarise(monto = sum(monto_crc, na.rm = TRUE),
              .groups = "drop")
  
  # Crear la tabla de los 12 meses
  tabla <- tibble(mes = 1:12) |>
    left_join(montos.mensuales, by = "mes") |>
    mutate(monto = replace_na(monto, 0),
           factor = (1 + tasa.mensual)^(12 - mes))
  
  # Calcular el valor acumulado
  va <- sum(tabla$monto * tabla$factor)
  
  # Calcular los intereses ganados
  interes <- va - sum(tabla$monto)
  
  # Devolver los resultados
  return(list(
    tabla = tabla,
    tasa.mensual = tasa.mensual,
    va = va,
    interes = interes
  ))
}






