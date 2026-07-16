#_______________________________________________________________________________
# maps.R
#
# Coordinación de Análisis de la Economía Laboral (CAEL) — Conasami
# Salario Digno — mapas coropléticos por entidad y ámbito
#
# Propósito: generar los mapas de los cuatro rubros (canasta de alimentos, canasta
#   de vivienda, NANV y salario digno neto) por entidad y ámbito, en paneles urbano
#   y rural. Clasifica cada rubro en cinco categorías de monto y aplica una rampa
#   tipo semáforo (verde montos bajos -> rojo/guinda montos altos) armonizada con la
#   paleta institucional DT 2026.
#
# Inputs:  salario_digno.csv (raíz; salida del script 4)
# Outputs (graphs/maps/, PNG 300 dpi + SVG):
#   mapa_alimentos_{urbano,rural,combined}
#   mapa_vivienda_{urbano,rural,combined}
#   mapa_nanv_{urbano,rural,combined}
#   mapa_wage_{urbano,rural,combined}
#
# Notas: paso 5 del pipeline (1 -> 2 -> 3 -> 4 -> maps); requiere salario_digno.csv.
#   Sigue el patrón de 000_representatividad.R (tema_mapa() + geom_sf + PNG/SVG). Las
#   filas "Nacional" no tienen geometría y se descartan solas en el join. Los cortes
#   de cada rubro son fijos (categorías de monto), no se recalculan con los datos.
#_______________________________________________________________________________

rm(list = ls()); gc()
options(scipen = 999)

if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  tidyverse,
  haven,
  data.table,
  patchwork,
  sf,
  rnaturalearth,
  rnaturalearthhires
)

source("scripts/theme_conasami_dt2026.R")

dir.create("graphs/maps", showWarnings = FALSE, recursive = TRUE)

# ── datos ──────────────────────────────────────────────────────────────────────

living_wage <- fread("salario_digno.csv") |>
  rename(
    salario_digno_neto   = salario_digno,
    alimentos_individual = canasta_alimentos,
    nanv                 = NANV,
    vivienda_familiar    = vivienda_mensual
  )

mexico <- ne_states(country = "Mexico", returnclass = "sf")
mexico$name <- ifelse(mexico$name == "Distrito Federal", "Ciudad de México", mexico$name)
# ne_states devuelve 33 features: una geometría degenerada (área 0, name = NA) que,
# de conservarse, aparece como categoría "NA" en la leyenda. Se descarta.
mexico <- mexico |> filter(!is.na(name))

# Geometría por ámbito; los nacionales no cruzan con ninguna entidad y se descartan.
sf_urbano <- mexico |> left_join(filter(living_wage, ambito == "Urbano"), by = c("name" = "nom_ent"))
sf_rural  <- mexico |> left_join(filter(living_wage, ambito == "Rural"),  by = c("name" = "nom_ent"))

# ── estética común ─────────────────────────────────────────────────────────────

# Rampa de 5 niveles (verde montos bajos -> guinda profundo montos altos), paleta
# histórica del proyecto. Las cinco bandas se retienen siempre, incluidas las vacías
# (drop = FALSE), para que la leyenda salga completa aunque un ámbito no use algún nivel.
rampa <- c("#A7E3A0", "#BFD39D", "#D6A79F", "#A84C5D", "#621132")

# Tema base para mapas (patrón guía DT 2026 §7.6): theme_void con fuente Noto Sans;
# no lleva rejilla cartesiana. El título de panel (Urbano/Rural) es un identificador
# estructural: se conserva restilizado como strip.text (Noto Sans bold, guinda
# profundo, centrado). La leyenda se reposiciona por panel en panel_mapa().
tema_mapa <- function() {
  theme_void(base_family = "Noto Sans") +
    theme(
      text             = element_text(color = conasami_neutros[["tinta"]]),
      plot.background  = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      legend.position     = "right",
      legend.key.height    = unit(0, "cm"),
      legend.key.width     = unit(0.5, "cm"),
      legend.key.spacing.y = unit(0, "cm"),
      legend.title         = element_blank(),
      legend.text          = element_text(family = "Noto Sans", size = 8,
                                           color = conasami_neutros[["tinta"]]),
      plot.title       = element_blank(),
      # Sin margen exterior: el mapa aprovecha todo el lienzo (queda banda blanca
      # vertical residual por la proporción ~1.5:1 de México en lienzo cuadrado).
      plot.margin      = margin(0, 0, 0, 0)
    )
}

# Clasifica un monto en categorías ordenadas a partir de cortes y etiquetas fijos.
clasificar <- function(x, breaks, labels) {
  factor(
    cut(x, breaks = breaks, labels = labels, right = FALSE, include.lowest = TRUE),
    levels = labels
  )
}

# Panel de un ámbito para un rubro ya clasificado en la columna `categoria`.
# La leyenda se construye desde una capa invisible (geom_point) que contiene las
# cinco bandas: geom_sf deja en blanco la casilla de las bandas sin ninguna entidad
# (drop = FALSE conserva la etiqueta pero no rellena la llave), así que se apaga su
# leyenda (show.legend = FALSE) y la capa auxiliar aporta las cinco casillas con su
# color. El punto va en tamaño 0 dentro del mapa (invisible) y solo alimenta la leyenda.
panel_mapa <- function(datos_sf, titulo, leyenda = TRUE) {
  bandas <- data.frame(
    categoria = factor(levels(datos_sf$categoria),
                       levels = levels(datos_sf$categoria))
  )
  ggplot(datos_sf) +
    geom_sf(aes(fill = categoria), color = "grey40", linewidth = 0.2,
            show.legend = FALSE) +
    geom_point(data = bandas, aes(fill = categoria),
               x = -102, y = 23, shape = 22, size = 0, stroke = 0,
               inherit.aes = FALSE) +
    scale_fill_manual(values = rampa, drop = FALSE,
                      na.value = "#F0F0F0", na.translate = FALSE) +
    guides(fill = guide_legend(override.aes = list(shape = 22, size = 4,
                                                   stroke = 0.2, color = "grey40"))) +
    coord_sf(datum = NA, expand = FALSE) +
    labs(title = titulo, fill = "") +
    tema_mapa() +
    theme(legend.position = if (leyenda) c(0.2, 0.2) else "none")
}

# Mapa de un rubro: clasifica, arma urbano + rural + combinado y exporta PNG (300
# dpi) + SVG de cada uno con los nombres de archivo históricos (DT 2026).
mapa_rubro <- function(col, breaks, labels, archivo) {
  u <- sf_urbano |> mutate(categoria = clasificar(.data[[col]], breaks, labels))
  r <- sf_rural  |> mutate(categoria = clasificar(.data[[col]], breaks, labels))

  p_urbano <- panel_mapa(u, "Urbano", leyenda = TRUE)
  p_rural  <- panel_mapa(r, "Rural",  leyenda = FALSE)
  combinado <- p_urbano + p_rural + plot_layout(ncol = 2)

  guardar_grafica_conasami(p_urbano,  paste0(archivo, "_urbano"),
                           tamano = "medio", dir = "graphs/maps")
  guardar_grafica_conasami(p_rural,   paste0(archivo, "_rural"),
                           tamano = "medio", dir = "graphs/maps")
  guardar_grafica_conasami(combinado, paste0(archivo, "_combined"),
                           tamano = "libre", width = 17.5, height = 9,
                           dir = "graphs/maps")

  invisible(combinado)
}

# ── configuración de rubros ──────────────────────────────────────────────────────
# Una fila por rubro: variable, cortes de monto, etiquetas y nombre de archivo.

rubros <- tribble(
  ~col,                    ~breaks,                                  ~labels,                                                                                          ~archivo,
  "alimentos_individual",  c(2000, 2500, 3000, 3500, 4000, Inf),     c("$2,000 - $2,500", "$2,500 - $3,000", "$3,000 - $3,500", "$3,500 - $4,000", "$4,000 o más"),    "mapa_alimentos",
  "vivienda_familiar",     c(2000, 3500, 4500, 5500, 6500, Inf),     c("$2,000 - $3,500", "$3,500 - $4,500", "$4,500 - $5,500", "$5,500 - $6,500", "$6,500 o más"),    "mapa_vivienda",
  "nanv",                  c(2800, 4500, 5000, 5500, 6000, Inf),     c("$2,800 - $4,500", "$4,500 - $5,000", "$5,000 - $5,500", "$5,500 - $6,000", "$6,000 o más"),    "mapa_nanv",
  "salario_digno_neto",    c(13000, 16000, 19000, 21000, 23000, Inf), c("$13,000 - $16,000", "$16,000 - $19,000", "$19,000 - $21,000", "$21,000 - $23,000", "$23,000 o más"), "mapa_wage"
)

pwalk(rubros, mapa_rubro)

message("maps.R: listo. Mapas por rubro en graphs/maps (urbano, rural y combinado; PNG + SVG).")
