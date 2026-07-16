#_______________________________________________________________________________
# 000_comparacion_indicadores_pobreza.R
#
# Coordinación de Análisis de la Economía Laboral (CAEL) — Conasami
# Salario Digno — script auxiliar del componente Alimentos
#
# Propósito: contrastar los Indicadores de Bienestar de Conasami (los que definen
#   el estrato de referencia de la Canasta de Alimentos) con los indicadores de
#   carencia de la medición multidimensional de pobreza de Coneval/INEGI, a lo
#   largo de la distribución del ingreso corriente per cápita. El ejercicio muestra
#   que los criterios de Conasami son más restrictivos (curvas por debajo de las de
#   carencia), congruente con la lógica del salario digno y la metodología Anker.
#
# Migra la parte de comparación de scripts/otros/alimentos/prueba_indicadores_pobreza.R
#   (se omiten el indicador de salud "Denisse" y el recálculo de la canasta).
#
# Alimenta la subsección "Comparación con los indicadores de pobreza multidimensional"
#   del documento extendido.
#
# Inputs:  data/enigh2024_ns_viviendas_dta/viviendas.dta
#          data/enigh2024_ns_poblacion_dta/poblacion.dta
#          data/enigh2024_ns_hogares_dta/hogares.dta
#          data/enigh2024_ns_concentradohogar_dta/concentradohogar.dta
#          data/pobreza_24.csv
#
# Outputs: finaldata/alimentos/10.comparacion_indicadores.csv  (largo: dimension,
#            percentil_nacional, fuente, pct_hogares)
#          finaldata/alimentos/11.cifras_comparacion.csv        (largo: dimension,
#            fuente, metrica, valor) para texto inline del documento
#          graphs/alimentos/comparacion_indicadores_pobreza.{png,eps}  (panel)
#          graphs/alimentos/comparacion_{alimentacion,vivienda,servicios,
#            educacion}.{png,eps}  (individuales)
#
# Notas: independiente del pipeline 1->4. El bloque de indicadores de bienestar se
#   duplica con 1.canasta_alimentos.R de forma deliberada para correr de manera
#   autónoma. La dimensión de salud requiere data/salud_cv_excel.csv (no incluido).
#_______________________________________________________________________________

rm(list = ls()); gc()
options(scipen = 999)

if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  tidyverse,
  haven,
  data.table,
  bit64,
  statar,
  patchwork
)

source("scripts/theme_conasami_dt2026.R")

dir.create("finaldata/alimentos", showWarnings = FALSE, recursive = TRUE)
dir.create("graphs/alimentos",    showWarnings = FALSE, recursive = TRUE)

guinda <- conasami_colores[["guinda"]]
gris   <- conasami_neutros[["gris"]]

# Exporta una gráfica en PNG (300 dpi) + SVG con la identidad DT 2026.
guardar_grafica <- function(plot, archivo, tamano = "libre",
                            width = 17.5, height = 9) {
  guardar_grafica_conasami(plot, archivo, tamano = tamano,
                           width = if (tamano == "libre") width else NULL,
                           height = if (tamano == "libre") height else NULL,
                           dir = "graphs/alimentos")
}

# ── indicadores de vivienda (bienestar Conasami) ─────────────────────────────────
# Reproduce 1.canasta_alimentos.R: i.materiales, i.saneamiento, i.agua.

vivienda <- read_dta("data/enigh2024_ns_viviendas_dta/viviendas.dta")

vivienda <- vivienda |>
  mutate(
    across(
      where(is.character) & !matches("folioviv"),
      ~ suppressWarnings(as.numeric(.))
    ),
    rural = ifelse(substr(folioviv, 3, 3) == "6", 1, 0),
    habitan_p_cuarto = tot_resid / cuart_dorm,
    agua_e = ifelse(agua_ent == 1 | agua_ent == 2, 1, 0),
    agua_ab = ifelse(ab_agua >= 1 &
                       ab_agua <= 4 |
                       ((ab_agua == 6 &
                           procaptar == 1) |
                          (agua_noe == 5) | (agua_noe == 6 & procaptar == 1)
                       ), 1, 0),
    agua_d = ifelse(dotac_agua == 1 | dotac_agua == 2, 1, 0),
    paredes = ifelse((mat_pared == 8) |
                       (mat_pared == 7 & rural == 1), 1, 0),
    techos = ifelse(mat_techos == 10 | mat_techos == 9, 1, 0),
    pisos = ifelse((mat_pisos == 2 | mat_pisos == 3), 1, 0),
    probms = ifelse((
      p_pandeos == 1 |
        p_levanta == 1 | p_humedad == 1 | p_fractura == 1
    ), 0, 1),
    wc = ifelse(excusado == 1 | excusado == 2, 1, 0),
    twc = ifelse(uso_compar == 2, 1, 0),
    awc = ifelse(sanit_agua == 1, 1, 0),
    red = ifelse(drenaje == 1, 1, 0)
  ) |>
  mutate(
    i.agua = ifelse(agua_e == 1 & agua_ab == 1 & agua_d == 1, 1, 0),
    i.materiales = ifelse(paredes == 1 & techos == 1 & pisos == 1 & probms == 1, 1, 0),
    i.hacinamiento = ifelse(habitan_p_cuarto <= 2.5, 1, 0),
    i.saneamiento = ifelse(wc == 1 & twc == 1 & awc == 1 & red == 1, 1, 0)
  )

indicadores_vivienda <- vivienda |>
  select(folioviv, starts_with("i."))

rm(vivienda)

# ── indicadores poblacionales (educación) ────────────────────────────────────────

poblacion <- read_dta("data/enigh2024_ns_poblacion_dta/poblacion.dta")

poblacion <- poblacion |>
  mutate(
    id.sin_edu_ini = ifelse(edad < 3 &
                              edu_ini == 6 &
                              no_asis %in% c(3, 5), 1, 0),
    id.sis_edu = ifelse(edad >= 3 & edad <= 18 & asis_esc == 2, 1, 0)
  )

indicadores_edu <- poblacion |>
  group_by(folioviv, foliohog) |>
  summarise(i.edu = ifelse(any(id.sin_edu_ini == 1 | id.sis_edu == 1), 0, 1)) |>
  ungroup()

rm(poblacion)

# ── indicadores del hogar (alimentación) ─────────────────────────────────────────

hogares <- read_dta("data/enigh2024_ns_hogares_dta/hogares.dta")

indicadores_alim <- hogares |>
  select(-c(acc_alim18)) |>
  mutate(i.alim = if_else(if_any(starts_with("acc_alim"), ~ .x == 1), 0, 1)) |>
  select(folioviv, foliohog, i.alim)

rm(hogares)

# ── consolidación de indicadores de bienestar ────────────────────────────────────

indicadores <- full_join(indicadores_vivienda, indicadores_edu, by = c("folioviv")) |>
  full_join(indicadores_alim, by = c("folioviv", "foliohog")) |>
  mutate(
    folioviv = as.integer64(folioviv),
    foliohog = as.integer64(foliohog)
  )

rm(indicadores_vivienda, indicadores_edu, indicadores_alim)

# ── indicadores de carencia de pobreza multidimensional (Coneval/INEGI) ───────────
# Se invierten a forma de satisfacción: 1 = el hogar NO presenta la carencia.

carencias <- fread("data/pobreza_24.csv") |>
  select(folioviv, foliohog, numren, starts_with("ic")) |>
  group_by(folioviv, foliohog) |>
  summarise(
    across(starts_with("ic"), ~ if_else(any(.x == 1), 0, 1)),
    .groups = "drop"
  ) |>
  select(folioviv, foliohog, ic_ali, ic_cv, ic_sbv, ic_rezedu) |>
  mutate(
    folioviv = as.integer64(folioviv),
    foliohog = as.integer64(foliohog)
  )

# ── concentrado y percentiles del ingreso corriente per cápita ───────────────────

concentrado <- read_dta("data/enigh2024_ns_concentradohogar_dta/concentradohogar.dta") |>
  mutate(
    folioviv = as.integer64(folioviv),
    foliohog = as.integer64(foliohog)
  ) |>
  left_join(carencias,   by = c("folioviv", "foliohog")) |>
  left_join(indicadores, by = c("folioviv", "foliohog")) |>
  rename(factor_exp = factor)   # col 8 = factor (verificado)

rm(carencias, indicadores)

concentrado <- concentrado |>
  mutate(ing_p = ing_cor / tot_integ) |>
  arrange(ing_p) |>
  mutate(percentil_nacional = xtile(ing_p, n = 100, wt = factor_exp))

# ── proporción ponderada que satisface cada criterio por percentil ───────────────
# Mapeo dimensión -> (indicador Conasami, indicador INEGI sin carencia).

mapeo <- tribble(
  ~dimension,           ~conasami,        ~inegi,
  "Alimentación",       "i.alim",         "ic_ali",
  "Vivienda (calidad)", "i.materiales",   "ic_cv",
  "Servicios básicos",  "i.saneamiento",  "ic_sbv",
  "Educación",          "i.edu",          "ic_rezedu"
)

prop_pond <- function(ind) {
  sum(concentrado$factor_exp[concentrado[[ind]] == 1], na.rm = TRUE) /
    sum(concentrado$factor_exp, na.rm = TRUE)
}

# proporción por percentil para un indicador dado
prop_percentil <- function(ind) {
  concentrado |>
    group_by(percentil_nacional) |>
    summarise(
      pct_hogares = sum(factor_exp[.data[[ind]] == 1], na.rm = TRUE) /
        sum(factor_exp, na.rm = TRUE),
      .groups = "drop"
    )
}

comparacion <- mapeo |>
  pmap_dfr(function(dimension, conasami, inegi) {
    bind_rows(
      prop_percentil(conasami) |> mutate(fuente = "CONASAMI"),
      prop_percentil(inegi)    |> mutate(fuente = "INEGI")
    ) |>
      mutate(dimension = dimension)
  }) |>
  mutate(
    dimension = factor(dimension, levels = mapeo$dimension),
    fuente    = factor(fuente, levels = c("CONASAMI", "INEGI"))
  ) |>
  select(dimension, percentil_nacional, fuente, pct_hogares)

fwrite(comparacion, "finaldata/alimentos/10.comparacion_indicadores.csv")

# ── gráficas (DT 2026): Conasami en guinda, INEGI en gris ────────────────────────

colores <- c("CONASAMI" = guinda, "INEGI" = gris)

# panel facetado por dimensión
g_panel <- comparacion |>
  ggplot(aes(x = percentil_nacional, y = pct_hogares * 100, color = fuente)) +
  geom_line() +
  geom_point(size = 0.8) +
  facet_wrap(~ dimension) +
  scale_color_manual(values = colores) +
  labs(color = "") +
  theme_conasami() +
  theme(strip.text = element_blank())

guardar_grafica(g_panel, "comparacion_indicadores_pobreza", width = 17.5, height = 11)

# individuales por dimensión
archivos_ind <- c(
  "Alimentación"       = "comparacion_alimentacion",
  "Vivienda (calidad)" = "comparacion_vivienda",
  "Servicios básicos"  = "comparacion_servicios",
  "Educación"          = "comparacion_educacion"
)

walk2(names(archivos_ind), archivos_ind, function(dim, archivo) {
  g <- comparacion |>
    filter(dimension == dim) |>
    ggplot(aes(x = percentil_nacional, y = pct_hogares * 100, color = fuente)) +
    geom_line() +
    geom_point(size = 0.9) +
    scale_color_manual(values = colores) +
    labs(color = "") +
    theme_conasami()
  guardar_grafica(g, archivo, tamano = "medio")
})

# ── cifras para la redacción del documento ───────────────────────────────────────
# Por dimensión y fuente: proporción nacional, en p10 y p90, y hogares que cumplen.

total_hogares <- sum(concentrado$factor_exp, na.rm = TRUE)

prop_en_percentil <- function(ind, p) {
  comparacion_ind <- prop_percentil(ind)
  comparacion_ind |> filter(percentil_nacional == p) |> pull(pct_hogares)
}

cifras_comparacion <- mapeo |>
  pmap_dfr(function(dimension, conasami, inegi) {
    tribble(
      ~dimension, ~fuente,     ~metrica,         ~valor,
      dimension,  "CONASAMI",  "pct_nacional",   prop_pond(conasami) * 100,
      dimension,  "INEGI",     "pct_nacional",   prop_pond(inegi) * 100,
      dimension,  "CONASAMI",  "pct_p10",        prop_en_percentil(conasami, 10) * 100,
      dimension,  "INEGI",     "pct_p10",        prop_en_percentil(inegi, 10) * 100,
      dimension,  "CONASAMI",  "pct_p90",        prop_en_percentil(conasami, 90) * 100,
      dimension,  "INEGI",     "pct_p90",        prop_en_percentil(inegi, 90) * 100,
      dimension,  "CONASAMI",  "hogares_cumplen", prop_pond(conasami) * total_hogares,
      dimension,  "INEGI",     "hogares_cumplen", prop_pond(inegi) * total_hogares
    )
  })

fwrite(cifras_comparacion, "finaldata/alimentos/11.cifras_comparacion.csv")

message("000_comparacion_indicadores_pobreza.R: listo. Outputs en finaldata/alimentos y graphs/alimentos.")
