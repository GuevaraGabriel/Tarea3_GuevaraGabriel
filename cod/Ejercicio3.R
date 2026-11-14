# Ejercicio 3: Valor acumulado e intereses

library(tidyverse)

# Cargar nuestras funciones
source("cod/funciones_tarea3.R")

# Leer las transacciones
transacciones <- read_delim(
  "data/transacciones_2025.csv",
  delim = ";",
  show_col_types = FALSE
)

# Leer los parametros personales
parametros <- read_delim(
  "config/parametros_tarea3.csv",
  delim = ";",
  col_types = cols(.default = col_character())
)

# Obtener la tasa y el canal personales
tasa.anual <- as.numeric(
  parametros$valor[parametros$parametro == "tasa.anual"]
)

canal.interes <- parametros$valor[
  parametros$parametro == "canal.interes"
]


# A y B: Parametros personales

resultado.personal <- valor.acumulado(
  transacciones,
  tasa.anual,
  canal.interes
)

resultado.3a <- resultado.personal$va
resultado.3b <- resultado.personal$interes

# C: Parametros comunes

resultado.comun <- valor.acumulado(
  transacciones,
  tasa.anual = 0.06,
  canal = "App"
)

resultado.3c <- resultado.comun$va

# resultados

cat(
  "RESULTADO_3A = ", sprintf("%.2f", resultado.3a), "\n",
  "RESULTADO_3B = ", sprintf("%.2f", resultado.3b), "\n",
  "RESULTADO_3C = ", sprintf("%.2f", resultado.3c), "\n",
  sep = ""
)

# Revisar la tabla mensual
print(resultado.personal$tabla)
