#_______________________________________________________________________________

# Título: Asequibilidad de la vivienda por percentil de ingreso

# Objetivo:

  # Ilustrar el supuesto del componente NANV: el gasto en vivienda es un bien
  # normal respecto al ingreso. Se grafica el porcentaje de hogares que destinan
  # más del 30% de su ingreso corriente al gasto en vivienda (aquellos que NO
  # cumplen el criterio de asequibilidad de ONU-Hábitat) por percentil nacional
  # de ingreso. La serie crece hacia la cola derecha de la distribución, lo que
  # sustenta la relación positiva entre vivienda e ingreso descrita en el texto.

# Autor:

  # Coordinación de Análisis de la Economía Laboral
  # de la Comisión Nacional de los Salarios Mínimos

# inputs:  data/enigh2024_ns_concentradohogar_dta/concentradohogar.dta
# outputs: graphs/relacion/asequibilidad.png

# Nota:
  # Independiente del pipeline. Alimenta la introducción de la sección
  # "Canasta NANV" del documento extendido (figura @fig-asequibilidad).

#_______________________________________________________________________________

rm(list = ls()); gc()
options(scipen = 999)

if (!require(pacman))
  install.packages("pacman")

pacman::p_load(tidyverse, haven, statar, data.table)

source("scripts/theme_conasami_dt2026.R")

# ── Carga y preparación ───────────────────────────────────────────────────────

concentrado <- read_dta(
  "data/enigh2024_ns_concentradohogar_dta/concentradohogar.dta"
)

concentrado <- concentrado |>
  rename(factor_exp = factor) |>
  mutate(
    # Gasto en vivienda con el mismo criterio del proyecto (alquiler + alquiler
    # estimado + servicios) para ser consistente con el componente de vivienda.
    viv                = vivienda + estim_alqu,
    no_asequible       = ifelse(viv / ing_cor > .30, 1, 0),
    percentil_nacional = xtile(ing_cor, n = 100, wt = factor_exp)
  ) |>
  filter(!is.na(no_asequible), ing_cor > 0)

# ── % de hogares no asequibles por percentil de ingreso ────────────────────────

tabla <- concentrado |>
  group_by(percentil_nacional) |>
  summarise(
    no_asequible = weighted.mean(no_asequible, w = factor_exp, na.rm = TRUE),
    .groups = "drop"
  )

# ── Gráfica ────────────────────────────────────────────────────────────────────

g_aseq <- ggplot(tabla, aes(x = percentil_nacional, y = no_asequible)) +
  geom_line(color = conasami_colores[["guinda"]], linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_conasami()

guardar_grafica_conasami(g_aseq, "asequibilidad", tamano = "ancho",
                         dir = "graphs/relacion")
