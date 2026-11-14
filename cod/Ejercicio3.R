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


# ============================================================
# Grafico del Ejercicio 3: Valor acumulado
# ============================================================

# Preparar los datos
datos.grafico <- resultado.personal$tabla |>
  mutate(
    acumulado = monto * factor
  ) |>
  select(mes, monto, acumulado) |>
  pivot_longer(
    cols = c(monto, acumulado),
    names_to = "tipo",
    values_to = "valor"
  ) |>
  mutate(
    tipo = recode(
      tipo,
      "monto" = "Monto original",
      "acumulado" = "Valor acumulado"
    )
  )

# Crear el grafico
grafico.ej3 <- ggplot(datos.grafico, aes(x = factor(mes), y = valor, fill = tipo)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c(
    "Monto original" = "steelblue",
    "Valor acumulado" = "darkorange"
  )) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = " mil millones")) +
  labs(
    title = "Valor acumulado de las transacciones",
    subtitle = paste("Canal:", canal.interes, "| Tasa anual:", tasa.anual * 100, "%"),
    x = "Mes",
    y = "Monto en colones",
    fill = "Tipo de monto"
  ) +
  theme_minimal()

# Mostrar grafico
print(grafico.ej3)

# Guardar imagen
dir.create("info/figuras",
           recursive = TRUE,
           showWarnings = FALSE)

ggsave(
  "info/figuras/ej3_acumulado.png",
  plot = grafico.ej3,
  width = 11,
  height = 6,
  dpi = 300
)
