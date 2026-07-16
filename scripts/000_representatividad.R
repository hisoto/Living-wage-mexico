#_______________________________________________________________________________
# 000_representatividad.R
#
# Coordinación de Análisis de la Economía Laboral (CAEL) — Conasami
# Salario Digno — script auxiliar de Metodología
#
# Propósito: análisis de representatividad estadística de la ENIGH 2024 para cada
#   zona de representatividad (entidad x ámbito + nacionales). Estima media, error
#   estándar y coeficiente de variación (CV) del gasto en alimentos, vivienda, NANV
#   (residual) y gasto corriente, clasifica la confianza según criterios INEGI y
#   genera el mapa del gasto corriente medio. El CV por rubro se reporta en el CSV,
#   que alimenta la tabla de representatividad del anexo del documento extendido.
#
# Alimenta la subsección "Representatividad estadística" del documento extendido.
#
# Inputs:  data/enigh2024_ns_concentradohogar_dta/concentradohogar.dta
# Outputs: finaldata/metodologia/representatividad_cv.csv
#          graphs/metodologia/mapa_gasto_corriente.{png,eps}
#
# Notas: independiente del pipeline 1->4 (solo requiere concentradohogar). El bloque
#   de carga y etiquetado se duplica con 000_hogar_modelo.R de forma deliberada para
#   que cada script corra de manera autónoma. El residual NANV usa gasto observado de
#   la ENIGH (gasto_mon - vivienda - alimentos), no las canastas estimadas.
#_______________________________________________________________________________

rm(list = ls()); gc()
options(scipen = 999)

if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  haven,
  tidyverse,
  survey,
  srvyr,
  sf,
  patchwork,
  rnaturalearth,
  rnaturalearthhires
)

source("scripts/theme_conasami_dt2026.R")

dir.create("finaldata/metodologia", showWarnings = FALSE, recursive = TRUE)
dir.create("graphs/metodologia",    showWarnings = FALSE, recursive = TRUE)

# ── carga y etiquetado ─────────────────────────────────────────────────────────

concentrado <- read_dta("data/enigh2024_ns_concentradohogar_dta/concentradohogar.dta") |>
  mutate(
    gasto_mon = gasto_mon / 3,   # trimestre -> mes
    alimentos = alimentos / 3,
    vivienda  = vivienda / 3,
    cve_ent   = as.numeric(substring(folioviv, 1, 2)),
    nom_ent   = case_when(
      cve_ent == 1  ~ "Aguascalientes",
      cve_ent == 2  ~ "Baja California",
      cve_ent == 3  ~ "Baja California Sur",
      cve_ent == 4  ~ "Campeche",
      cve_ent == 5  ~ "Coahuila",
      cve_ent == 6  ~ "Colima",
      cve_ent == 7  ~ "Chiapas",
      cve_ent == 8  ~ "Chihuahua",
      cve_ent == 9  ~ "Ciudad de México",
      cve_ent == 10 ~ "Durango",
      cve_ent == 11 ~ "Guanajuato",
      cve_ent == 12 ~ "Guerrero",
      cve_ent == 13 ~ "Hidalgo",
      cve_ent == 14 ~ "Jalisco",
      cve_ent == 15 ~ "México",
      cve_ent == 16 ~ "Michoacán",
      cve_ent == 17 ~ "Morelos",
      cve_ent == 18 ~ "Nayarit",
      cve_ent == 19 ~ "Nuevo León",
      cve_ent == 20 ~ "Oaxaca",
      cve_ent == 21 ~ "Puebla",
      cve_ent == 22 ~ "Querétaro",
      cve_ent == 23 ~ "Quintana Roo",
      cve_ent == 24 ~ "San Luis Potosí",
      cve_ent == 25 ~ "Sinaloa",
      cve_ent == 26 ~ "Sonora",
      cve_ent == 27 ~ "Tabasco",
      cve_ent == 28 ~ "Tamaulipas",
      cve_ent == 29 ~ "Tlaxcala",
      cve_ent == 30 ~ "Veracruz",
      cve_ent == 31 ~ "Yucatán",
      cve_ent == 32 ~ "Zacatecas"
    ),
    rural = if_else(substr(folioviv, 3, 3) == "6", 1, 0)
  )

disenio <- concentrado |>
  as_survey_design(weights = factor, strata = est_dis, ids = upm)

# ── media, SE y CV por zona de representatividad ─────────────────────────────────

# Las cuatro estimaciones se repiten por entidad x ámbito y a nivel nacional por
# ámbito; se factoriza en una función para no duplicar el bloque de summarise().
estimar_rubros <- function(design) {
  design |>
    summarise(
      alimentos_media       = survey_mean(alimentos, na.rm = TRUE, vartype = c("se", "cv")),
      vivienda_media        = survey_mean(vivienda, na.rm = TRUE, vartype = c("se", "cv")),
      nanv_media            = survey_mean(gasto_mon - vivienda - alimentos, na.rm = TRUE, vartype = c("se", "cv")),
      gasto_corriente_media = survey_mean(gasto_mon, na.rm = TRUE, vartype = c("se", "cv"))
    )
}

medias_ent <- disenio |>
  group_by(nom_ent, rural) |>
  estimar_rubros() |>
  ungroup()

medias_nac <- disenio |>
  group_by(rural) |>
  estimar_rubros() |>
  ungroup() |>
  mutate(nom_ent = "Nacional", ambito = if_else(rural == 1, "Rural", "Urbano"))

medias_tot <- disenio |>
  estimar_rubros() |>
  mutate(nom_ent = "Nacional", ambito = "Total", rural = NA_real_)

medias <- medias_ent |>
  mutate(ambito = if_else(rural == 1, "Rural", "Urbano")) |>
  bind_rows(medias_nac, medias_tot) |>
  relocate(nom_ent, ambito) |>
  arrange(nom_ent, ambito)

# survey_mean() nombra el estimador puntual como <rubro>_media y agrega <rubro>_media_se
# y <rubro>_media_cv; se renombran a sufijos limpios antes de pasar a formato largo.
representatividad <- medias |>
  rename_with(~ str_replace(.x, "_media_se$", "_se"), ends_with("_media_se")) |>
  rename_with(~ str_replace(.x, "_media_cv$", "_cv"), ends_with("_media_cv")) |>
  rename_with(~ str_replace(.x, "_media$", "_mean"), ends_with("_media")) |>
  select(-rural) |>
  pivot_longer(
    cols = matches("_(mean|se|cv)$"),
    names_to = c("rubro", ".value"),
    names_pattern = "(.*)_(mean|se|cv)$"
  ) |>
  mutate(
    rubro = recode(rubro,
      alimentos       = "Alimentos",
      vivienda        = "Vivienda",
      nanv            = "NANV",
      gasto_corriente = "Gasto corriente"
    ),
    confianza = case_when(
      cv < 0.15 ~ "Alta",
      cv <= 0.30 ~ "Media",
      TRUE ~ "Baja"
    )
  ) |>
  arrange(nom_ent, ambito, rubro)

write_csv(representatividad, "finaldata/metodologia/representatividad_cv.csv")

# ── mapas ────────────────────────────────────────────────────────────────────────

mexico <- ne_states(country = "Mexico", returnclass = "sf")
mexico$name <- ifelse(mexico$name == "Distrito Federal", "Ciudad de México", mexico$name)

# Geometría por ámbito con las medias unidas (se excluyen los nacionales).
medias_mapa <- medias |>
  rename_with(~ str_replace(.x, "_media$", "_mean"), ends_with("_media")) |>
  filter(nom_ent != "Nacional")

sf_urbano <- mexico |> left_join(filter(medias_mapa, ambito == "Urbano"), by = c("name" = "nom_ent"))
sf_rural  <- mexico |> left_join(filter(medias_mapa, ambito == "Rural"),  by = c("name" = "nom_ent"))

# Tema base para mapas (patrón guía DT 2026 §7.6): theme_void con fuente Noto Sans.
# El título de panel (Urbano/Rural) es un identificador estructural: se restiliza
# como strip.text (Noto Sans bold, guinda profundo, centrado). La barra de gradiente
# va DENTRO del mapa (esquina inferior izquierda, sobre el océano Pacífico, como en
# maps.R) para que el mapa aproveche todo el ancho del lienzo; cada panel conserva
# su propia barra porque el rango del gradiente difiere entre ámbitos.
tema_mapa <- function() {
  theme_void(base_family = "Noto Sans") +
    theme(
      text             = element_text(color = conasami_neutros[["tinta"]]),
      plot.background  = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      legend.position  = c(0.17, 0.28),
      legend.key.size  = unit(0.4, "cm"),
      legend.title     = element_blank(),
      legend.text      = element_text(family = "Noto Sans", size = 9,
                                      color = conasami_neutros[["tinta"]]),
      plot.title       = element_blank(),
      # Sin margen exterior: el mapa aprovecha todo el lienzo.
      plot.margin      = margin(0, 0, 0, 0)
    )
}

mapa_base <- function(datos_sf, col, titulo, etiquetas_fill = waiver()) {
  ggplot(datos_sf) +
    geom_sf(aes(fill = .data[[col]]), color = "grey40", linewidth = 0.2) +
    scale_fill_gradient(low = "#FDE9EF",
                        high = conasami_colores[["guinda_profundo"]],
                        labels = etiquetas_fill) +
    coord_sf(datum = NA, expand = FALSE) +
    labs(title = titulo, fill = "") +
    tema_mapa()
}

# Mapa de un rubro: exporta el combinado urbano + rural (nombre histórico, lo consume
# el documento extendido) más un mapa individual por ámbito. Como la escala es un
# gradiente continuo con rango propio por panel, cada mapa individual conserva su
# leyenda. Individuales en tamaño medio (8×8 cm, Manual DT 2026); PNG (300 dpi) + SVG.
guardar_mapa <- function(col, archivo, etiquetas_fill = waiver()) {
  p_urbano <- mapa_base(sf_urbano, col, "Urbano", etiquetas_fill)
  p_rural  <- mapa_base(sf_rural,  col, "Rural",  etiquetas_fill)
  panel    <- p_urbano + p_rural + plot_layout(ncol = 2)

  guardar_grafica_conasami(panel, archivo, tamano = "libre",
                           width = 17.5, height = 9,
                           dir = "graphs/metodologia")
  guardar_grafica_conasami(p_urbano, paste0(archivo, "_urbano"),
                           tamano = "medio", dir = "graphs/metodologia")
  guardar_grafica_conasami(p_rural,  paste0(archivo, "_rural"),
                           tamano = "medio", dir = "graphs/metodologia")
  panel
}

# Mapa del gasto corriente medio (contraste norte-sur)
guardar_mapa("gasto_corriente_mean", "mapa_gasto_corriente",
             etiquetas_fill = scales::label_dollar(big.mark = ","))

message("000_representatividad.R: listo. Tabla (CSV) y mapa de gasto corriente en finaldata/metodologia y graphs/metodologia.")
