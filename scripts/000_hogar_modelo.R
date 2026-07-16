#_______________________________________________________________________________
# 000_hogar_modelo.R
#
# Coordinación de Análisis de la Economía Laboral (CAEL) — Conasami
# Salario Digno — script auxiliar de Metodología
#
# Propósito: estadísticas de composición del hogar de la ENIGH 2024 por entidad y
#   ámbito (integrantes, personas trabajadoras y razón de dependencia), que sustentan
#   la selección del hogar modelo (4 integrantes, 2 personas trabajadoras).
#
# Alimenta la subsección "Selección del tamaño y composición del hogar modelo" del
#   documento extendido.
#
# Inputs:  data/enigh2024_ns_concentradohogar_dta/concentradohogar.dta
# Outputs: finaldata/metodologia/hogar_composicion.csv
#          graphs/metodologia/composicion_hogar.{png,eps}
#          graphs/metodologia/ratio_hogar.{png,eps}
#
# Notas: independiente del pipeline 1->4. El bloque de carga y etiquetado se duplica
#   con 000_representatividad.R de forma deliberada para correr de manera autónoma.
#_______________________________________________________________________________

rm(list = ls()); gc()
options(scipen = 999)

if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  haven,
  tidyverse,
  survey,
  srvyr,
  patchwork
)

source("scripts/theme_conasami_dt2026.R")

dir.create("finaldata/metodologia", showWarnings = FALSE, recursive = TRUE)
dir.create("graphs/metodologia",    showWarnings = FALSE, recursive = TRUE)

# ── carga y etiquetado ─────────────────────────────────────────────────────────

concentrado <- read_dta("data/enigh2024_ns_concentradohogar_dta/concentradohogar.dta") |>
  mutate(
    cve_ent = as.numeric(substring(folioviv, 1, 2)),
    nom_ent = case_when(
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

# ── composición del hogar por zona de representatividad ──────────────────────────

estimar_hogar <- function(design) {
  design |>
    summarise(
      integrantes  = survey_mean(tot_integ, na.rm = TRUE, vartype = c("se", "cv")),
      trabajadoras = survey_mean(ocupados, na.rm = TRUE, vartype = c("se", "cv"))
    )
}

hogares_ent <- disenio |>
  group_by(nom_ent, rural) |>
  estimar_hogar() |>
  ungroup()

hogares_nac <- disenio |>
  group_by(rural) |>
  estimar_hogar() |>
  ungroup() |>
  mutate(nom_ent = "Nacional")

hogares <- bind_rows(hogares_nac, hogares_ent) |>
  mutate(
    ambito = if_else(rural == 1, "Rural", "Urbano"),
    ratio  = integrantes / trabajadoras,   # razón de dependencia (integrantes por persona trabajadora)
    grupo_color = case_when(
      nom_ent == "Nacional" & rural == 0 ~ "Nacional_Urbano",
      nom_ent == "Nacional" & rural == 1 ~ "Nacional_Rural",
      rural == 0 ~ "Urbano",
      rural == 1 ~ "Rural"
    )
  ) |>
  relocate(nom_ent, ambito) |>
  arrange(nom_ent, ambito)

# Promedio nacional total (ambos ámbitos), solo para la tabla; no entra en las gráficas.
hogar_total <- disenio |>
  estimar_hogar() |>
  mutate(nom_ent = "Nacional", ambito = "Total", ratio = integrantes / trabajadoras)

bind_rows(
  hogar_total,
  hogares
) |>
  select(nom_ent, ambito, integrantes, integrantes_cv,
         trabajadoras, trabajadoras_cv, ratio) |>
  write_csv("finaldata/metodologia/hogar_composicion.csv")

# ── gráficas ─────────────────────────────────────────────────────────────────────

# Ámbitos en colores institucionales; los nacionales se distinguen con tonos más
# oscuros de la misma familia (guinda profundo / gris secundario).
colores_hogar <- c(
  "Nacional_Urbano" = unname(conasami_colores[["guinda_profundo"]]),
  "Nacional_Rural"  = unname(conasami_neutros[["texto_secundario"]]),
  "Urbano"          = unname(conasami_colores[["guinda"]]),
  "Rural"           = unname(conasami_neutros[["gris"]])
)

# Barras por entidad y ámbito; los nacionales se distinguen con tonos propios.
# El título es el identificador estructural del subpanel; se restiliza como
# strip.text del theme. Ejes/títulos de eje viven en el pie de figura.
barra_hogar <- function(var, titulo, y_lab = "Número de personas", breaks_n = 4, limites = NULL) {
  ggplot(hogares) +
    geom_bar(
      aes(x = reorder(nom_ent, {{ var }} * (rural == 1)), y = {{ var }}, fill = grupo_color),
      stat = "identity", position = "dodge"
    ) +
    geom_hline(
      yintercept = mean(pull(hogares, {{ var }})),
      linetype = "dashed", color = conasami_colores[["guinda_profundo"]], linewidth = 0.5
    ) +
    scale_fill_manual(
      values = colores_hogar,
      breaks = c("Urbano", "Rural"),
      labels = c("Urbano", "Rural")
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = breaks_n), limits = limites) +
    theme_conasami() +
    labs(title = NULL, fill = "") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          plot.title = element_text(family = "Noto Sans", face = "bold",
                                    size = 10, hjust = 0.5,
                                    color = conasami_colores[["guinda_profundo"]]))
}

guardar_grafica <- function(plot, archivo, width = 17.5, height = 9) {
  guardar_grafica_conasami(plot, archivo, tamano = "libre",
                           width = width, height = height,
                           dir = "graphs/metodologia")
}

plot_integrantes <- barra_hogar(integrantes, "Integrantes del hogar")
plot_trabajadoras <- barra_hogar(trabajadoras, "Personas trabajadoras", limites = c(0, 4))

composicion <- plot_integrantes + plot_trabajadoras + plot_layout(ncol = 2, nrow = 1)
guardar_grafica(composicion, "composicion_hogar")

# Paneles individuales en tamaño mediano (8 x 8 cm), sin título (viven en el pie de figura).
guardar_grafica_conasami(plot_integrantes,  "composicion_hogar_integrantes",
                         tamano = "medio", dir = "graphs/metodologia")
guardar_grafica_conasami(plot_trabajadoras, "composicion_hogar_trabajadoras",
                         tamano = "medio", dir = "graphs/metodologia")

plot_ratio <- barra_hogar(ratio, "Razón de dependencia", y_lab = "Integrantes por persona trabajadora", breaks_n = 2)
guardar_grafica(plot_ratio, "ratio_hogar")

message("000_hogar_modelo.R: listo. Tabla y gráficas en finaldata/metodologia y graphs/metodologia.")
