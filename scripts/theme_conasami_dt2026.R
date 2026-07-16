# ─────────────────────────────────────────────────────────────────────────────
# theme_conasami_dt2026.R  —  Dirección Técnica CONASAMI, Manual DT 2026 (v1.0)
#
# FUENTE CANÓNICA (raíz CAEL). Este archivo es el master del tema gráfico de la
# CAEL: se copia idéntico al directorio de scripts de cada proyecto que produce
# gráficas. NO editar las copias locales — todo cambio se hace aquí y se re-copia
# (ver CLAUDE.md raíz §4.1 y GUIA_GRAFICAS_DT2026.md §10).
#
# Proyectos que llevan una copia idéntica de este archivo (código byte a byte):
#   · Informes/comportamiento_precios
#   · Informes/Negociación laboral
#   · Informes/Politica Social
#   · investigaciones/inflacion_nivel_ingreso
#   · investigaciones/negociación colectiva INEGI
#
# Filosofía (Manual §6, §7, §11): R produce SOLO el área de trazado. Antetítulo,
# título, detalles y fuente NO se generan aquí — viven en la tabla-envoltorio de
# Word. La leyenda SÍ se conserva y se estiliza.
#
# Tipografía: ragg + systemfonts. showtext acoplaba el tamaño del texto a su dpi
# global y al dispositivo elegido por ggsave, lo que producía letra ~⅓ más
# pequeña en Positron (96 dpi en pantalla) que en una sesión de terminal
# (300 dpi). ragg::agg_png renderiza a puntos reales de forma determinista —
# idéntico en Positron y en terminal — resolviendo las fuentes con systemfonts.
#
# Salida: PNG (raster, 300 dpi, transparente) + SVG (vectorial, transparente).
# El SVG reemplaza al EPS anterior: soporta transparencia, es editable en
# Inkscape / Illustrator y se inserta sin pérdida en Word vía "Imagen vectorial".
#
# Requiere: ggplot2, ragg, systemfonts, svglite, here.
# ─────────────────────────────────────────────────────────────────────────────

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(ggplot2, ragg, systemfonts, svglite, here)

# ── tipografía (systemfonts; render por ragg) ────────────────────────────────
# "Noto Sans" está instalada en la carpeta de usuario y systemfonts la resuelve
# por nombre de familia. Para los numerales de eje y de leyenda se usa la
# variante "Noto Sans Tab" (definida abajo): Noto Sans con cifras TABULARES.
#
# NOTA (cero plano): NO se usa "Noto Sans Mono". Su cero por defecto lleva barra
# diagonal y NINGÚN feature OpenType la desactiva (probados zero=0/1, ss01–ss10,
# cv01–cv10). Noto Sans sí trae el cero plano; con cifras tabulares iguala el
# ancho de los dígitos, recuperando la alineación que el Manual §11.1 buscaba
# con la mono (ver GUIA_GRAFICAS_DT2026.md y nota editorial en el Manual §11.1).

.cnsm_user_fonts <- file.path(Sys.getenv("LOCALAPPDATA"),
                              "Microsoft", "Windows", "Fonts")
.cnsm_win_fonts  <- file.path(Sys.getenv("SystemRoot"), "Fonts")

# Registra una familia con systemfonts solo si el SO no la expone ya.
.cnsm_register_font <- function(family, plain, bold = NULL,
                                italic = NULL, bolditalic = NULL) {
  if (family %in% systemfonts::system_fonts()$family) return(invisible(TRUE))
  if (is.null(plain) || !file.exists(plain)) {
    warning("No se encontró la fuente '", family,
            "'; ggplot usará una sustituta.", call. = FALSE)
    return(invisible(FALSE))
  }
  systemfonts::register_font(
    name       = family,
    plain      = plain,
    bold       = if (!is.null(bold)       && file.exists(bold))       bold       else plain,
    italic     = if (!is.null(italic)     && file.exists(italic))     italic     else plain,
    bolditalic = if (!is.null(bolditalic) && file.exists(bolditalic)) bolditalic else plain
  )
  invisible(TRUE)
}

# Noto Sans: respaldo por si el SO no la expusiera (normalmente sí).
.cnsm_register_font(
  "Noto Sans",
  plain      = file.path(.cnsm_user_fonts, "NotoSans-Regular.ttf"),
  bold       = file.path(.cnsm_user_fonts, "NotoSans-Bold.ttf"),
  italic     = file.path(.cnsm_user_fonts, "NotoSans-Italic.ttf"),
  bolditalic = file.path(.cnsm_user_fonts, "NotoSans-BoldItalic.ttf")
)

# "Noto Sans Tab": variante de Noto Sans con cifras TABULARES, para los numerales
# de eje y de leyenda. Da cero PLANO (a diferencia de Noto Sans Mono) y dígitos
# de ancho uniforme, conservando la alineación tabular del Manual §11.1.
systemfonts::register_variant(
  "Noto Sans Tab",
  family   = "Noto Sans",
  features = systemfonts::font_feature(numbers = "tabular")
)

# ── paleta institucional ─────────────────────────────────────────────────────
# Orden categórico FIJO del Manual (§11.1). Úsese como default para series sin
# significado +/−.
cnsm_pal <- c(
  "#9B2247",  # 1  guinda institucional (primario)
  "#1E5B4F",  # 2  verde institucional
  "#A57F2C",  # 3  dorado
  "#611232",  # 4  guinda profundo
  "#002F2A",  # 5  verde profundo
  "#E6D194",  # 6  arena
  "#98989A",  # 7  gris
  "#161A1D"   # 8  tinta
)

# Acceso por nombre semántico. NOTA: para análisis con dirección (+/−), la
# convención del usuario es verde = positivo/incremento y guinda = negativo/
# decremento; el dorado se reserva para el INPC general.
conasami_colores <- c(
  guinda          = "#9B2247",
  verde           = "#1E5B4F",
  dorado          = "#A57F2C",
  guinda_profundo = "#611232",
  verde_profundo  = "#002F2A",
  arena           = "#E6D194",
  gris            = "#98989A",
  tinta           = "#161A1D"
)

# Neutros de apoyo (texto, reglas, fondos, eje base).
conasami_neutros <- c(
  tinta            = "#161A1D",  # texto principal
  texto_secundario = "#6B6863",  # bajadas, etiquetas, notas
  gris             = "#98989A",
  regla            = "#D8D3C7",
  regla_suave      = "#ECE8DE",  # rejilla horizontal
  papel_calido     = "#FBFAF7",
  blanco           = "#FFFFFF",
  eje_base         = "#BCB7A9"   # línea de eje x
)

# Convención de dirección (+/−): verde = positivo/incremento,
# guinda = negativo/decremento. Centraliza los colores usados en las series.
direccion_colores <- c(
  Positivo = unname(conasami_colores[["verde"]]),
  Negativo = unname(conasami_colores[["guinda"]])
)

# Rampa SECUENCIAL para quintiles de ingreso (preserva el orden Q1→Q5).
# Q1 (ingreso bajo) = arena claro  →  Q5 (ingreso alto) = guinda profundo.
cnsm_quintil_pal <- grDevices::colorRampPalette(
  c("#E6D194", "#A57F2C", "#9B2247", "#611232")
)(5)
names(cnsm_quintil_pal) <- as.character(1:5)

# ── theme principal ──────────────────────────────────────────────────────────
# Tamaños tipográficos alineados al §11.1 del Manual DT 2026 (base 10, ejes 8 pt,
# leyenda 9 pt, títulos de panel 10 pt).
theme_conasami <- function(base_size = 10) {
  theme_minimal(base_size = base_size, base_family = "Noto Sans") +
    theme(
      # Elementos que viven en Word: se desactivan en R
      plot.title    = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption  = element_blank(),
      axis.title    = element_blank(),

      # Fondos transparentes
      panel.background = element_blank(),
      plot.background  = element_blank(),

      # Rejilla: solo horizontal, regla suave
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = conasami_neutros[["regla_suave"]],
                                        linewidth = 0.3),

      # Ejes
      axis.line.x = element_line(color = conasami_neutros[["eje_base"]], linewidth = 0.3),
      axis.line.y = element_blank(),
      axis.ticks  = element_blank(),
      axis.text   = element_text(family = "Noto Sans Tab",
                                 size = 8, color = conasami_neutros[["texto_secundario"]]),

      # Leyenda (se conserva y estiliza; las gráficas sin leyenda simplemente no
      # mapean una variable a color/fill)
      legend.position   = "bottom",
      legend.title      = element_text(family = "Noto Sans", size = base_size - 1,
                                       color = conasami_neutros[["texto_secundario"]]),
      legend.text       = element_text(family = "Noto Sans",
                                       size = base_size - 1,
                                       color = conasami_neutros[["tinta"]]),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.key        = element_blank(),

      # Títulos de panel (§6.4 del Manual): Noto Sans 10 pt Negrita, guinda
      # profundo, centrado.
      strip.background = element_blank(),
      strip.text       = element_text(family = "Noto Sans", face = "bold",
                                      size = base_size,
                                      color = conasami_colores[["guinda_profundo"]],
                                      hjust = 0.5),

      plot.margin = margin(6, 12, 6, 6)
    )
}

# ── escalas de color/relleno ─────────────────────────────────────────────────
# Categóricas (orden fijo del Manual) — para series sin orden +/− ni de ingreso.
scale_color_conasami <- function(...) scale_color_manual(values = unname(cnsm_pal), ...)
scale_fill_conasami  <- function(...) scale_fill_manual(values = unname(cnsm_pal), ...)
# Alias cortos compatibles con la nomenclatura del Manual
scale_color_cnsm <- scale_color_conasami
scale_fill_cnsm  <- scale_fill_conasami

# Dirección +/− (verde = positivo, guinda = negativo).
scale_color_direccion <- function(...) scale_color_manual(values = direccion_colores, ...)
scale_fill_direccion  <- function(...) scale_fill_manual(values = direccion_colores, ...)

# Secuenciales por quintil — para datos con orden de ingreso (Q1→Q5).
scale_color_quintil <- function(...) scale_color_manual(values = cnsm_quintil_pal, ...)
scale_fill_quintil  <- function(...) scale_fill_manual(values = cnsm_quintil_pal, ...)

# ── helper de exportación (PNG 300 dpi + SVG) ────────────────────────────────
# tamano = "ancho" → 17.5 × 8 cm (ancho completo, ~6 años de datos)
# tamano = "medio" → 8 × 8 cm     (medio ancho, ~3 años de datos)
# tamano = "libre" → width × height explícitos en cm (figuras densas: heatmaps,
#                    rankings con muchas categorías). Ancho recomendado: 17.5 cm.
# width/height = override opcional en cm aun con tamano "ancho"/"medio".
# dest   = carpeta opcional (p. ej. la de Word); si se indica, copia ahí el PNG.
# svg    = FALSE permite suprimir la salida vectorial (solo PNG para previews).
#
# PNG: device = ragg::agg_png → tamaño de texto determinista (mismo resultado en
# Positron y en terminal). SVG: device = svglite::svglite → vectorial, transparente,
# fuentes resueltas vía systemfonts (texto seleccionable / editable).
guardar_grafica_conasami <- function(plot, archivo,
                                     tamano = c("ancho", "medio", "libre"),
                                     width  = NULL,
                                     height = NULL,
                                     dir    = here::here("graphs"),
                                     dpi    = 300,
                                     dest   = NULL,
                                     svg    = TRUE) {
  tamano <- match.arg(tamano)
  dims <- switch(
    tamano,
    ancho = c(17.5, 8),
    medio = c(8, 8),
    libre = {
      if (is.null(width) || is.null(height))
        stop("tamano = 'libre' requiere width y height (en cm).")
      c(width, height)
    }
  )
  if (!is.null(width))  dims[1] <- width
  if (!is.null(height)) dims[2] <- height
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  ruta_png <- file.path(dir, paste0(archivo, ".png"))
  ggsave(ruta_png, plot = plot, width = dims[1], height = dims[2],
         units = "cm", dpi = dpi, device = ragg::agg_png,
         background = "transparent")

  ruta_svg <- NULL
  if (isTRUE(svg)) {
    ruta_svg <- file.path(dir, paste0(archivo, ".svg"))
    ggsave(ruta_svg, plot = plot, width = dims[1], height = dims[2],
           units = "cm", device = svglite::svglite, bg = "transparent")
  }

  # Copia opcional a una carpeta externa (p. ej. la de Word del pipeline mensual).
  if (!is.null(dest) && dir.exists(dest))
    file.copy(ruta_png, file.path(dest, basename(ruta_png)), overwrite = TRUE)

  invisible(c(png = ruta_png, svg = ruta_svg))
}

# ── ejemplos de uso (comentados) ─────────────────────────────────────────────
# # Barras verticales — width = 0.68 (barGap 0.32)
# ggplot(df, aes(mes, valor)) +
#   geom_col(fill = cnsm_pal[1], width = 0.68) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
#   theme_conasami()
#
# # Líneas — grosor 0.75, esquinas redondas, marcador en cada punto
# ggplot(df, aes(mes, valor, group = serie, color = serie)) +
#   geom_line(linewidth = 0.75, lineend = "round", linejoin = "round") +
#   geom_point(size = 1.4) +
#   scale_color_conasami() +
#   theme_conasami()
#
# # Exportar (genera PNG 300 dpi + SVG en graphs/)
# guardar_grafica_conasami(last_plot(),
#                          "inflacion_quintiles_2026m03", tamano = "ancho")
