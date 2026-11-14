# Cargar nuestras funciones
source("cod/funciones_tarea3.R")

# Probar la funcion benford.mad
prueba <- benford.mad(c(100, 100, 100))

print(prueba)

library(tidyverse)

# Importar las transacciones
transacciones <- read_delim(
  "data/transacciones_2025.csv",
  delim = ";",
  show_col_types = FALSE
)

# Importar los cantones
cantones <- read_csv(
  "base/cantones.csv",
  show_col_types = FALSE
)

# Cargar las funciones actualizadas
source("cod/funciones_tarea3.R")

# Probar la funcion con SINPE Movil
prueba.cantones <- benford.canton(
  transacciones,
  canal = "SINPE Móvil",
  cantones = cantones
)

#a
# Mostrar las primeras filas
head(prueba.cantones)

# Seleccionar los montos de SINPE Movil
montos.sinpe <- transacciones |>
  filter(canal == "SINPE Móvil") |>
  pull(monto_crc)

# Calcular el MAD general
resultado.2a <- benford.mad(montos.sinpe)

print(round(resultado.2a, 4))

#b
resultado.2b <- prueba.cantones |>
  slice_max(order_by = mad, n = 1, with_ties = FALSE) |>
  pull(canton)

print(resultado.2b)

#c
resultado.2c <- sum(prueba.cantones$mad > 0.015, na.rm = TRUE)

print(resultado.2c)

# Calcular el MAD por canton en el canal Web
prueba.web <- benford.canton(
  transacciones,
  canal = "Web",
  cantones = cantones
)

#d
# Obtener el canton con mayor MAD
resultado.2d <- prueba.web |>
  slice_max(order_by = mad, n = 1, with_ties = FALSE) |>
  pull(canton)

print(resultado.2d)
