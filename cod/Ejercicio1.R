library(tidyverse)

transacciones <- read_delim(
  "data/transacciones_2025.csv",
  delim = ";"
)

glimpse(transacciones)


conteo.diario <- transacciones |>
  count(fecha, name = "n.tx") |>
  arrange(fecha)

glimpse(conteo.diario)

# Promedio de los primeros 7 dias
mean(conteo.diario$n.tx[1:7])

# Desviacion estandar de los primeros 7 dias
sd(conteo.diario$n.tx[1:7])

# Limite de alerta para el 8 de enero
limite <- mean(conteo.diario$n.tx[1:7]) +
  2 * sd(conteo.diario$n.tx[1:7])

print(limite)

# Comprobar si el 8 de enero tiene alerta
print(conteo.diario$n.tx[8] > limite)

# Cargar nuestras funciones
source("cod/funciones_tarea3.R")

# Probar la función de alertas
prueba <- alerta.movil(
  transacciones,
  ventana = 7,
  k.alerta = 2
)

glimpse(prueba)

print(head(prueba, 9))

#===============================================================================

# Ejercicio 1A: cantidad de días con alerta
resultado.1a <- sum(prueba$alerta, na.rm = TRUE)

print(resultado.1a)

#===============================================================================

# Ejercicio 1B: fecha de la primera alerta
resultado.1b <- prueba |>
  filter(alerta == TRUE) |>
  slice_head(n = 1) |>
  pull(fecha)

print(resultado.1b)

#===============================================================================

# Ejercicio 1C: alertas con parametros comunes
prueba.comun <- alerta.movil(transacciones, ventana = 7, k.alerta = 2)

resultado.1c <- sum(prueba.comun$alerta, na.rm = TRUE)

print(resultado.1c)


  