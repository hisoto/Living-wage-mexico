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
# Outputs (graphs/maps/, PNG 300 dpi + EPS):
#   mapa_alimentos_{urbano,rural,combined}
#   mapa_vivienda_{urbano,rural,combined}
#   mapa_nanv_{urbano,rural,combined}
#   mapa_wage_{urbano,rural,combined}
#
# Notas: paso 5 del pipeline (1 -> 2 -> 3 -> 4 -> maps); requiere salario_digno.csv.
#   Sigue el patrón de 000_representatividad.R (tema_mapa() + geom_sf + PNG/EPS). Las
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

source("scripts/theme_conasami.R")

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

# Rampa de 5 niveles tipo semáforo (verde = montos bajos -> rojo/guinda = montos altos),
# armonizada con la paleta institucional DT 2026. Punto medio luminoso (amarillo) para
# distinguir bien las cinco bandas, incluso las vacías retenidas por drop = FALSE.
rampa <- c("#2E7D5B", "#9CCB6A", "#F4D35E", "#E08A3C", "#B23A48")

# Tema base para mapas: hereda fuente y leyenda de theme_conasami() y elimina los
# elementos cartesianos (ejes, grid, borde) impropios de un mapa.
tema_mapa <- function() {
  theme_conasami() +
    theme(
      axis.line        = element_blank(),
      axis.ticks       = element_blank(),
      axis.text        = element_blank(),
      axis.title       = element_blank(),
      panel.border     = element_blank(),
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = "transparent", color = NA),
      legend.position  = "right",
      legend.key.size  = unit(0.4, "cm"),
      plot.title       = element_text(size = 14, face = "bold", hjust = 0.5)
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
panel_mapa <- function(datos_sf, titulo, leyenda = TRUE) {
  ggplot(datos_sf) +
    geom_sf(aes(fill = categoria), color = "grey40", linewidth = 0.2) +
    scale_fill_manual(values = rampa, drop = FALSE,
                      na.value = "#F0F0F0", na.translate = FALSE) +
    coord_sf(datum = NA) +
    labs(title = titulo, fill = "") +
    tema_mapa() +
    theme(legend.position = if (leyenda) c(0.2, 0.2) else "none")
}

# Mapa de un rubro: clasifica, arma urbano + rural + combinado y exporta PNG (300
# dpi) y EPS de cada uno con los nombres de archivo históricos.
mapa_rubro <- function(col, breaks, labels, archivo, width = 10, height = 5) {
  u <- sf_urbano |> mutate(categoria = clasificar(.data[[col]], breaks, labels))
  r <- sf_rural  |> mutate(categoria = clasificar(.data[[col]], breaks, labels))

  p_urbano <- panel_mapa(u, "Urbano", leyenda = TRUE)
  p_rural  <- panel_mapa(r, "Rural",  leyenda = FALSE)
  combinado <- p_urbano + p_rural + plot_layout(ncol = 2)

  guardar <- function(plot, sufijo, w, h) {
    ggsave(file.path("graphs/maps", paste0(archivo, "_", sufijo, ".png")),
           plot, width = w, height = h, dpi = 300)
    ggsave(file.path("graphs/maps", paste0(archivo, "_", sufijo, ".eps")),
           plot, width = w, height = h, device = cairo_ps)
  }

  guardar(p_urbano,  "urbano",   width / 2, height)
  guardar(p_rural,   "rural",    width / 2, height)
  guardar(combinado, "combined", width,     height)

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

message("maps.R: listo. Mapas por rubro en graphs/maps (urbano, rural y combinado; PNG + EPS).")
