#_______________________________________________________________________________
# 1.canasta_alimentos.R
#
# Coordinación de Análisis de la Economía Laboral (CAEL) — Conasami
# Salario Digno — componente alimentario de la Canasta Digna
#
# Propósito: construir la Canasta de Alimentos. (1) define indicadores de bienestar
#   multidimensional, (2) selecciona el estrato poblacional de referencia por ámbito,
#   (3) elige productos y cantidades medianas representativas, (4) calcula precios
#   medianos por entidad/ámbito y (5) valora la canasta aplicando el factor 1.20.
#
# Inputs:  data/enigh2024_ns_viviendas_dta/viviendas.dta
#          data/enigh2024_ns_poblacion_dta/poblacion.dta
#          data/enigh2024_ns_hogares_dta/hogares.dta
#          data/enigh2024_ns_concentradohogar_dta/concentradohogar.dta
#          data/enigh2024_ns_gastoshogar_dta/gastoshogar.dta
#
# Outputs (finaldata/alimentos/):
#   1.indicadores_bienestar.csv             hogares sin cada indicador (nivel nacional)
#   2.percentiles_indicadores_bienestar.csv % que cumple cada indicador por percentil
#   3.tabla_indice_bienestar.csv            criterios unión/intermedio/intersección por decil
#   4.tabla_medias_estrato.csv              gasto medio del estrato de referencia
#   5.cantidades_ca_{rural,urbano}.csv      cantidades medianas por producto
#   6.precios_ca_{rural,urbano}.csv         precios medianos y valor por producto
#   7.canasta_alimentaria.csv               valor de la canasta por entidad y ámbito
#   8.cifras_texto.csv                      cifras que alimentan la redacción del documento
#   9.comparacion_lpei.csv                  canasta nacional vs LPEI de Coneval (ago-2024)
#
# Gráficas (graphs/alimentos/, PNG 300 dpi + EPS):
#   indicadores_bienestar  canasta_entidad  canasta_vs_lpei
#
# Notas: el componente alimenta a los scripts 3 y 4 vía 7.canasta_alimentaria.csv;
#   no modificar los nombres de salida sin actualizar a los consumidores. Parámetros
#   congelados: factor 1.20, estrato urbano p41-61, estrato rural p80-100, productos
#   con incidencia >=10% (urbano) y >=8% (rural).
#_______________________________________________________________________________

rm(list = ls()); gc()
options(scipen = 999)

if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  tidyverse,
  haven,
  Hmisc,
  EnvStats,
  survey,
  srvyr,
  data.table,
  bit64,
  statar,
  patchwork,
  tidytext
)

source("scripts/theme_conasami.R")

dir.create("finaldata/alimentos", showWarnings = FALSE, recursive = TRUE)
dir.create("graphs/alimentos",    showWarnings = FALSE, recursive = TRUE)

# Línea de Pobreza Extrema por Ingresos (Coneval, agosto 2024), pesos mensuales por
# persona. Referencia externa para la validación ex post de suficiencia; no se recalcula.
# Fuente: Coneval, líneas de pobreza por ingresos. [Verificar antes de publicar]
lpei_rural  <- 1800.55
lpei_urbano <- 2354.65

# ── catálogo de entidades ────────────────────────────────────────────────────
# (clave INEGI -> nombre); se reutiliza para precios y para la canasta final.

nombres_entidad <- function(cve) {
  case_when(
    cve == 1  ~ "Aguascalientes",
    cve == 2  ~ "Baja California",
    cve == 3  ~ "Baja California Sur",
    cve == 4  ~ "Campeche",
    cve == 5  ~ "Coahuila",
    cve == 6  ~ "Colima",
    cve == 7  ~ "Chiapas",
    cve == 8  ~ "Chihuahua",
    cve == 9  ~ "Ciudad de México",
    cve == 10 ~ "Durango",
    cve == 11 ~ "Guanajuato",
    cve == 12 ~ "Guerrero",
    cve == 13 ~ "Hidalgo",
    cve == 14 ~ "Jalisco",
    cve == 15 ~ "México",
    cve == 16 ~ "Michoacán",
    cve == 17 ~ "Morelos",
    cve == 18 ~ "Nayarit",
    cve == 19 ~ "Nuevo León",
    cve == 20 ~ "Oaxaca",
    cve == 21 ~ "Puebla",
    cve == 22 ~ "Querétaro",
    cve == 23 ~ "Quintana Roo",
    cve == 24 ~ "San Luis Potosí",
    cve == 25 ~ "Sinaloa",
    cve == 26 ~ "Sonora",
    cve == 27 ~ "Tabasco",
    cve == 28 ~ "Tamaulipas",
    cve == 29 ~ "Tlaxcala",
    cve == 30 ~ "Veracruz",
    cve == 31 ~ "Yucatán",
    cve == 32 ~ "Zacatecas"
  )
}

# Diseño muestral complejo de la ENIGH (estrato + UPM).
disenio <- function(.data, pesos) {
  .data |>
    as_survey_design(weights = {{ pesos }}, strata = est_dis, ids = upm)
}

# Exporta una gráfica en PNG (300 dpi) y EPS, alineada al Manual DT 2026.
guardar_grafica <- function(plot, archivo, width = 10, height = 5) {
  ggsave(file.path("graphs/alimentos", paste0(archivo, ".png")),
         plot, width = width, height = height, dpi = 300)
  ggsave(file.path("graphs/alimentos", paste0(archivo, ".eps")),
         plot, width = width, height = height, device = cairo_ps)
}

# ── indicadores de vivienda ────────────────────────────────────────────────────

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
    # acceso a agua entubada dentro de la vivienda o del terreno
    i.agua = ifelse(agua_e == 1 & agua_ab == 1 & agua_d == 1, 1, 0),
    # materiales adecuados
    i.materiales = ifelse(paredes == 1 & techos == 1 & pisos == 1 & probms == 1, 1, 0),
    # sin hacinamiento
    i.hacinamiento = ifelse(habitan_p_cuarto <= 2.5, 1, 0),
    # retrete sin uso compartido y con drenaje a red pública
    i.saneamiento = ifelse(wc == 1 & twc == 1 & awc == 1 & red == 1, 1, 0)
  )

indicadores_vivienda <- vivienda |>
  select(folioviv, rural, starts_with("i."))

rm(vivienda)

# ── indicadores poblacionales (educación) ───────────────────────────────────────

poblacion <- read_dta("data/enigh2024_ns_poblacion_dta/poblacion.dta")

poblacion <- poblacion |>
  mutate(
    # niños sin educación inicial por falta de recursos o sin acceso
    id.sin_edu_ini = ifelse(edad < 3 &
                              edu_ini == 6 &
                              no_asis %in% c(3, 5), 1, 0),
    # infancias que no asisten a la escuela
    id.sis_edu = ifelse(edad >= 3 & edad <= 18 & asis_esc == 2, 1, 0),
    # infancias con alfabetismo
    id.alfabetismo = ifelse(edad >= 7 & edad <= 18 & alfabetism == 2, 1, 0)
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
  full_join(indicadores_alim, by = c("folioviv", "foliohog"))

rm(indicadores_vivienda, indicadores_edu, indicadores_alim)

concentrado <- read_dta("data/enigh2024_ns_concentradohogar_dta/concentradohogar.dta") |>
  left_join(indicadores, by = c("folioviv", "foliohog")) |>
  mutate(
    i.exceso.g.alim = ifelse(alimentos / gasto_mon >= .75, 0, 1),
    rural = ifelse(substr(folioviv, 3, 3) == 6, 1, 0)
  ) |>
  rename(factor_exp = factor)   # col 8 = factor (verificado)

rm(indicadores)

tabla.indicadores <- concentrado |>
  summarise(
    hogares = sum(factor_exp, na.rm = TRUE),
    i.agua = sum(factor_exp[i.agua == 0], na.rm = TRUE),
    i.materiales = sum(factor_exp[i.materiales == 0], na.rm = TRUE),
    i.hacinamiento = sum(factor_exp[i.hacinamiento == 0], na.rm = TRUE),
    i.saneamiento = sum(factor_exp[i.saneamiento == 0], na.rm = TRUE),
    i.edu = sum(factor_exp[i.edu == 0], na.rm = TRUE),
    i.alim = sum(factor_exp[i.alim == 0], na.rm = TRUE),
    i.exceso.g.alim = sum(factor_exp[i.exceso.g.alim == 0], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 0))) |>
  pivot_longer(cols = -hogares, names_to = "indicador", values_to = "cantidad")

fwrite(tabla.indicadores, "finaldata/alimentos/1.indicadores_bienestar.csv")

# ── percentiles del ingreso corriente per cápita ─────────────────────────────────

concentrado <- concentrado |>
  mutate(ing_p = (ing_cor) / tot_integ) |>
  arrange(ing_p) |>
  # percentil nacional: para gráficas e identificación de indicadores
  mutate(percentil_nacional = xtile(ing_p, n = 100, wt = factor_exp)) |>
  group_by(rural) |>
  # percentil por ámbito: el estrato se selecciona por ámbito
  mutate(percentil = xtile(ing_p, n = 100, wt = factor_exp)) |>
  ungroup()

tabla.percentil.indicadores <- concentrado |>
  group_by(percentil_nacional) |>
  summarise(
    i.agua.p = sum(factor_exp[i.agua == 1], na.rm = TRUE) / sum(factor_exp, na.rm = TRUE),
    i.materiales.p = sum(factor_exp[i.materiales == 1], na.rm = TRUE) / sum(factor_exp, na.rm = TRUE),
    i.hacinamiento.p = sum(factor_exp[i.hacinamiento == 1], na.rm = TRUE) / sum(factor_exp, na.rm = TRUE),
    i.saneamiento.p = sum(factor_exp[i.saneamiento == 1], na.rm = TRUE) / sum(factor_exp, na.rm = TRUE),
    i.edu.p = sum(factor_exp[i.edu == 1], na.rm = TRUE) / sum(factor_exp, na.rm = TRUE),
    i.alim.p = sum(factor_exp[i.alim == 1], na.rm = TRUE) / sum(factor_exp, na.rm = TRUE),
    i.exceso.g.alim.p = sum(factor_exp[i.exceso.g.alim == 1], na.rm = TRUE) / sum(factor_exp, na.rm = TRUE)
  )

fwrite(tabla.percentil.indicadores,
       "finaldata/alimentos/2.percentiles_indicadores_bienestar.csv")

# ── gráfica de indicadores de bienestar (DT 2026) ────────────────────────────────
# Los indicadores seleccionados para el índice se resaltan en guinda institucional.

guinda <- "#611232"
gris   <- "#98989A"

# series: vector con nombres = etiqueta de la serie, valores = columna del df.
# valores: vector con nombres = etiqueta, valores = color. La serie seleccionada va en guinda.
panel_indicador <- function(datos, series, valores, titulo) {
  datos |>
    select(percentil_nacional, all_of(unname(series))) |>
    pivot_longer(-percentil_nacional, names_to = "col", values_to = "p") |>
    mutate(serie = factor(names(series)[match(col, series)], levels = names(series))) |>
    ggplot(aes(x = percentil_nacional, y = p * 100, color = serie)) +
    geom_line() +
    geom_point() +
    scale_color_manual(values = valores) +
    labs(title = titulo, x = "Percentil", y = "(%) de hogares", color = "") +
    theme_conasami() +
    theme(legend.position = "bottom")
}

g_alimento <- panel_indicador(
  tabla.percentil.indicadores,
  c("Sin falta de alimentos" = "i.alim.p", "Exceso de gasto alimento" = "i.exceso.g.alim.p"),
  c("Sin falta de alimentos" = guinda, "Exceso de gasto alimento" = gris),
  "Indicadores alimentarios"
)

g_vivienda <- panel_indicador(
  tabla.percentil.indicadores,
  c("Habitabilidad" = "i.materiales.p", "Sin hacinamiento" = "i.hacinamiento.p"),
  c("Habitabilidad" = guinda, "Sin hacinamiento" = gris),
  "Indicadores vivienda"
)

g_servicios <- panel_indicador(
  tabla.percentil.indicadores,
  c("Servicios de baño" = "i.saneamiento.p", "Acceso a fuentes de agua" = "i.agua.p"),
  c("Servicios de baño" = guinda, "Acceso a fuentes de agua" = gris),
  "Indicadores servicios"
)

g_edu <- panel_indicador(
  tabla.percentil.indicadores,
  c("Sin rezago educativo" = "i.edu.p"),
  c("Sin rezago educativo" = guinda),
  "Indicador de educación"
)

g_indicadores <- g_alimento + g_vivienda + g_servicios + g_edu +
  plot_layout(ncol = 2, nrow = 2)

guardar_grafica(g_indicadores, "indicadores_bienestar", width = 12, height = 8)

rm(tabla.percentil.indicadores, g_alimento, g_vivienda, g_servicios, g_edu, g_indicadores)

# ── índice de bienestar y criterios de selección ─────────────────────────────────

concentrado <- concentrado |>
  mutate(indice = (i.saneamiento + i.materiales + i.edu + i.alim))

design <- concentrado |>
  as_survey_design(weights = factor_exp, strata = est_dis, ids = upm)

resultados_svyr <- svyby(
  # criterio unión (>=1), intermedio (>=2) e intersección (==4) por decil y ámbito
  ~ I(indice >= 2) + I(indice >= 1) + I(indice == 4),
  by = ~ cut(
    percentil,
    breaks = quantile(concentrado$percentil, probs = seq(0, 1, 0.1)),
    labels = 1:10,
    include.lowest = TRUE
  ) + rural,
  design = design,
  FUN = svymean,
  na.rm = TRUE
)

resultados_svyr <- resultados_svyr |>
  rename(
    percentil = 1,
    criterio_intermedio = 4,
    criterio_union = 6,
    criterio_interseccion = 8
  ) |>
  select(percentil, rural, criterio_intermedio, criterio_union, criterio_interseccion) |>
  mutate(
    criterio_intermedio = round(criterio_intermedio * 100, 2),
    criterio_union = round(criterio_union * 100, 2),
    criterio_interseccion = round(criterio_interseccion * 100, 2)
  )

fwrite(resultados_svyr, "finaldata/alimentos/3.tabla_indice_bienestar.csv")

rm(resultados_svyr, design)

# ── estrato de referencia ────────────────────────────────────────────────────────

estrato <- concentrado |>
  filter((rural == 0 & percentil >= 41 & percentil <= 61) |
           (rural == 1 & percentil >= 80 & percentil <= 100)) |>
  select(folioviv, foliohog, rural, alimentos, gasto_mon, ing_cor, est_dis, upm, factor_exp) |>
  mutate(estrato_ref = 1)

rm(concentrado)

# gasto medio del estrato

options(survey.lonely.psu = "adjust")

tabla.medias <- estrato |>
  disenio(factor_exp) |>
  group_by(rural) |>
  summarise(
    n = n(),
    alimentos = survey_mean(alimentos / 3, vartype = c("se", "cv", "ci"), level = 0.95, na.rm = TRUE),
    gasto_mon = survey_mean(gasto_mon / 3, vartype = c("se", "cv", "ci"), level = 0.95, na.rm = TRUE),
    ing_cor   = survey_mean(ing_cor / 3,   vartype = c("se", "cv", "ci"), level = 0.95, na.rm = TRUE),
    n_hogares = survey_total(1,            vartype = c("se", "cv", "ci"), level = 0.95, na.rm = TRUE)
  )

num_hog_urbano <- tabla.medias |> filter(rural == 0) |> pull(n_hogares)
num_hog_rural  <- tabla.medias |> filter(rural == 1) |> pull(n_hogares)

n_muestra_urbano <- tabla.medias |> filter(rural == 0) |> pull(n)
n_muestra_rural  <- tabla.medias |> filter(rural == 1) |> pull(n)

fwrite(tabla.medias, "finaldata/alimentos/4.tabla_medias_estrato.csv")

rm(tabla.medias)

estrato <- estrato |>
  select(folioviv, foliohog, estrato_ref)

# ── productos representativos ────────────────────────────────────────────────────

claves <- c(
  "111111", "111112", "111113", "111114", "111115", "111116",
  "181171", "181113", "181156", "181114", "181153", "181115",
  "181116", "181117", "181158", "181118", "181119", "181122",
  "181121", "181120", "181124", "181125", "181126", "181123",
  "181127", "181157", "181128", "181129", "181130", "181132",
  "181131", "181160", "181133", "181159", "181163", "181141",
  "181165", "181142", "181164", "181161", "181166", "181140",
  "181167", "181136", "181137", "181138", "181135", "181139",
  "181134", "181155", "181144", "181145", "181143", "181168",
  "111125", "111122", "111126", "111121", "111123", "111124",
  "181162", "181170"
)

gasto_hogar <- read_dta("data/enigh2024_ns_gastoshogar_dta/gastoshogar.dta") |>
  left_join(estrato, by = c("folioviv", "foliohog")) |>
  rename(factor_exp = factor) |>
  mutate(rural = ifelse(substr(folioviv, 3, 3) == "6", 1, 0),
         clase = substr(clave, 1, 2)) |>
  filter(estrato_ref == 1, clase == "01" | clave %in% claves)

rm(estrato)

gasto_hogar_ind <- gasto_hogar |>
  distinct(folioviv, foliohog, clave, .keep_all = TRUE) |>
  mutate(consumio = 1)

productos_survey <- gasto_hogar_ind |>
  disenio(factor_exp) |>
  group_by(rural, clave) |>
  summarise(pct_hogares = survey_total(consumio, vartype = c("se", "cv", "ci"), level = 0.95),
            .groups = "drop")

productos <- productos_survey |>
  mutate(porcentaje = ifelse(rural == 1,
                             pct_hogares / num_hog_rural,
                             pct_hogares / num_hog_urbano)) |>
  filter(porcentaje >= 0.1 & rural == 0 | porcentaje >= 0.08 & rural == 1)

ca_urbano <- productos |> filter(rural == 0) |> pull(clave)
ca_rural  <- productos |> filter(rural == 1) |> pull(clave)

rm(productos_survey, gasto_hogar_ind, productos)

# ── cantidades medianas consumidas por el estrato ────────────────────────────────

design <- gasto_hogar |> disenio(factor_exp)

cantidades_urbano <- design |>
  filter(rural == 0, clave %in% ca_urbano) |>
  group_by(clave) |>
  summarise(cantidad = survey_median(cantidad, na.rm = TRUE,
                                     vartype = c("se", "cv", "ci"), level = 0.95))

cantidades_rural <- design |>
  filter(rural == 1, clave %in% ca_rural) |>
  group_by(clave) |>
  summarise(cantidad = survey_median(cantidad, na.rm = TRUE,
                                     vartype = c("se", "cv", "ci"), level = 0.95))

fwrite(cantidades_rural,  "finaldata/alimentos/5.cantidades_ca_rural.csv")
fwrite(cantidades_urbano, "finaldata/alimentos/5.cantidades_ca_urbano.csv")

rm(design, gasto_hogar)

# ── estimación del precio ────────────────────────────────────────────────────────
# costo medio implícito (gasto/cantidad); se remueven atípicos por IQR dentro de
# cada producto x entidad x ámbito y se toma la mediana ponderada.

gasto_hogar <- read_dta("data/enigh2024_ns_gastoshogar_dta/gastoshogar.dta") |>
  mutate(
    rural = ifelse(substr(folioviv, 3, 3) == "6", "rural", "urbano"),
    cve_ent = as.numeric(entidad),
    nom_ent = nombres_entidad(cve_ent),
    costo_medio = gasto / cantidad
  ) |>
  group_by(clave, nom_ent, rural) |>
  mutate(
    IQR = IQR(costo_medio, na.rm = TRUE),
    Q1 = quantile(costo_medio, 0.25, na.rm = TRUE),
    Q3 = quantile(costo_medio, 0.75, na.rm = TRUE),
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR
  ) |>
  filter(costo_medio >= Lower_Bound & costo_medio <= Upper_Bound) |>
  ungroup() |>
  select(-IQR, -Q1, -Q3, -Lower_Bound, -Upper_Bound)

ca_list   <- list(rural = ca_rural,          urbano = ca_urbano)
cant_list <- list(rural = cantidades_rural,  urbano = cantidades_urbano)

estimar_precios <- function(amb) {
  gc()

  design <- gasto_hogar |>
    filter(clave %in% ca_list[[amb]], rural == amb) |>
    as_survey_design(weights = factor, ids = upm, strata = est_dis)

  precios_nac <- design |>
    group_by(clave) |>
    summarise(precio = survey_median(costo_medio, na.rm = TRUE,
                                     vartype = c("se", "cv", "ci"), level = 0.95)) |>
    select(clave, precio) |>
    mutate(nom_ent = "Nacional")

  precios_ent <- design |>
    group_by(clave, nom_ent) |>
    summarise(precio = survey_median(costo_medio, na.rm = TRUE,
                                     vartype = c("se", "cv", "ci"), level = 0.95)) |>
    select(clave, nom_ent, precio)

  precios <- precios_nac |>
    rbind(precios_ent) |>
    left_join(cant_list[[amb]], by = "clave") |>
    mutate(valor = precio * cantidad)

  fwrite(precios, paste0("finaldata/alimentos/6.precios_ca_", amb, ".csv"))
  precios
}

resultados <- set_names(c("rural", "urbano")) |>
  map(estimar_precios)

rm(gasto_hogar)

# ── valor de la canasta alimentaria ──────────────────────────────────────────────
# factor 1.20 = 5% (desperdicio/condimentos) + 15% (variedad de alimentos), Anker.

canasta_ambito <- function(amb, etiqueta) {
  resultados[[amb]] |>
    group_by(nom_ent) |>
    summarise(valor = sum(valor)) |>
    mutate(canasta_alimentos = valor * 1.20, ambito = etiqueta)
}

canasta <- bind_rows(
  canasta_ambito("urbano", "Urbano"),
  canasta_ambito("rural",  "Rural")
) |>
  arrange(ambito, canasta_alimentos)

fwrite(canasta, "finaldata/alimentos/7.canasta_alimentaria.csv")

# ── gráfica de la canasta por entidad (DT 2026) ──────────────────────────────────

canasta_ent <- canasta |> filter(nom_ent != "Nacional")

g_canasta <- ggplot(canasta_ent,
                    aes(x = reorder(nom_ent, canasta_alimentos), y = canasta_alimentos,
                        fill = ambito)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  coord_flip() +
  scale_fill_manual(values = c("Urbano" = guinda, "Rural" = "#1E5B4F")) +
  labs(title = "Canasta alimentaria por entidad y ámbito",
       x = "", y = "Pesos mensuales por persona", fill = "") +
  theme_conasami() +
  theme(legend.position = "bottom")

guardar_grafica(g_canasta, "canasta_entidad", width = 9, height = 10)

# ── comparación canasta vs LPEI (validación ex post) ─────────────────────────────
# Se compara el valor nacional con la LPEI (se conserva para 9.comparacion_lpei.csv,
# que alimenta el documento) y se grafica cada entidad x ámbito contra la línea LPEI.

comparacion_lpei <- canasta |>
  filter(nom_ent == "Nacional") |>
  select(ambito, canasta = canasta_alimentos) |>
  mutate(lpei = ifelse(ambito == "Rural", lpei_rural, lpei_urbano))

fwrite(comparacion_lpei, "finaldata/alimentos/9.comparacion_lpei.csv")

# Valor de la LPEI por ámbito para la línea de referencia de cada panel.
lineas_lpei <- tibble(
  ambito = c("Rural", "Urbano"),
  lpei   = c(lpei_rural, lpei_urbano)
)

g_lpei <- canasta_ent |>
  ggplot(aes(x = reorder_within(nom_ent, canasta_alimentos, ambito),
             y = canasta_alimentos)) +
  geom_col(fill = guinda, width = 0.7) +
  geom_hline(data = lineas_lpei,
             aes(yintercept = lpei, linetype = "LPEI (Coneval)"),
             color = gris, linewidth = 0.7) +
  scale_x_reordered() +
  scale_linetype_manual(values = c("LPEI (Coneval)" = "dashed")) +
  facet_wrap(~ ambito, scales = "free_y") +
  coord_flip() +
  labs(title = "Canasta alimentaria por entidad y ámbito frente a la LPEI",
       x = "", y = "Pesos mensuales por persona", linetype = "") +
  theme_conasami() +
  theme(legend.position = "bottom")

guardar_grafica(g_lpei, "canasta_vs_lpei", width = 11, height = 9)

# ── cifras para la redacción del documento ───────────────────────────────────────

sin <- function(ind) {
  tabla.indicadores |> filter(indicador == ind) |> pull(cantidad)
}
total_hogares <- tabla.indicadores |> distinct(hogares) |> pull(hogares)

cifras_texto <- tribble(
  ~concepto,            ~valor,
  "total_hogares",      total_hogares,
  "sin_agua",           sin("i.agua"),
  "con_agua",           total_hogares - sin("i.agua"),
  "sin_materiales",     sin("i.materiales"),
  "con_materiales",     total_hogares - sin("i.materiales"),
  "sin_hacinamiento",   sin("i.hacinamiento"),
  "con_hacinamiento",   total_hogares - sin("i.hacinamiento"),
  "sin_saneamiento",    sin("i.saneamiento"),
  "con_saneamiento",    total_hogares - sin("i.saneamiento"),
  "sin_edu",            sin("i.edu"),
  "con_edu",            total_hogares - sin("i.edu"),
  "sin_alim",           sin("i.alim"),
  "con_alim",           total_hogares - sin("i.alim"),
  "n_muestra_urbano",   n_muestra_urbano,
  "n_muestra_rural",    n_muestra_rural,
  "n_muestra_total",    n_muestra_urbano + n_muestra_rural,
  "n_pob_urbano",       round(num_hog_urbano, 0),
  "n_pob_rural",        round(num_hog_rural, 0),
  "n_productos_urbano", length(ca_urbano),
  "n_productos_rural",  length(ca_rural)
)

fwrite(cifras_texto, "finaldata/alimentos/8.cifras_texto.csv")

message("1.canasta_alimentos.R: listo. Outputs en finaldata/alimentos y graphs/alimentos.")
