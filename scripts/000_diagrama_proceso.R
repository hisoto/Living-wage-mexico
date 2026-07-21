# ─────────────────────────────────────────────────────────────────────────────
# 000_diagrama_proceso.R — Exportación del diagrama de proceso del Salario Digno
#
# El diagrama es una pieza vectorial autorada a mano bajo la identidad DT 2026.
# Fuente de verdad (editable, tipografía referida por nombre):
#   graphs/diagrama/diagrama_canasta_digna_2026m07.svg   (flujo de 4 etapas, sin título)
#
# Este script NO redibuja el flujo; lo toma como master, incrusta Noto Sans
# (Regular/Bold/Italic como @font-face base64) y produce DOS variantes:
#
#   1) ANCHO (Word / reporte)  — 17.5 x 4.8 cm, sin título (va en la tabla-envoltorio).
#        · diagrama_canasta_digna_2026m07.png            (2067 px ≈ 300 dpi)
#
#   2) LÁMINA (presentación)   — 16:9 (1920x1080), con antetítulo, título y fuente
#        embebidos (una lámina no usa tabla-envoltorio). El flujo se reutiliza tal cual
#        (misma fuente de verdad), escalado y centrado sobre el lienzo.
#        · diagrama_canasta_digna_lamina_2026m07.svg     (autocontenida, editable)
#        · diagrama_canasta_digna_lamina_2026m07.png     (2560x1440 px)
#
# Para cambiar el texto de la banda de la lámina, editar lam_antetitulo/titulo/fuente.
# Para cambiar el flujo (etapas, íconos, viñetas), editar el SVG master y re-correr.
#
# Tabla-envoltorio de Word (variante ANCHO):
#   Antetítulo : CANASTA DIGNA
#   Título     : Proceso de estimación del Salario Digno
#   Fuente     : Fuente: Conasami con base en la ENIGH 2024 (INEGI)   (sin punto final)
#
# Requiere: rsvg, systemfonts, jsonlite, here.
# ─────────────────────────────────────────────────────────────────────────────

rm(list = ls()); gc()
options(scipen = 999)
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(rsvg, systemfonts, jsonlite, here)

# ── parámetros ───────────────────────────────────────────────────────────────
dir_out    <- here::here("graphs", "diagrama")
svg_master <- file.path(dir_out, "diagrama_canasta_digna_2026m07.svg")
ancho_px   <- 2067L   # 17.5 cm a 300 dpi  ->  round(17.5 / 2.54 * 300)
lamina_px  <- 2560L   # ancho del PNG 16:9 para láminas (2560 x 1440)

lam_antetitulo <- "CANASTA DIGNA"
lam_titulo     <- "Proceso de estimación del Salario Digno"
lam_fuente     <- "Fuente: Conasami con base en la ENIGH 2024 (INEGI)"

# ── localizar las variantes de Noto Sans ─────────────────────────────────────
# systemfonts resuelve por nombre; si cayera en una sustituta, se usa la copia de
# la carpeta de fuentes del usuario (misma ruta que theme_conasami_dt2026.R).
user_fonts <- file.path(Sys.getenv("LOCALAPPDATA"), "Microsoft", "Windows", "Fonts")
ttf_path <- function(archivo, weight = "normal", style = "normal") {
  p <- tryCatch(systemfonts::match_fonts("Noto Sans", weight = weight, style = style)$path,
                error = function(e) NULL)
  if (is.null(p) || !grepl("noto", basename(p), ignore.case = TRUE))
    p <- file.path(user_fonts, archivo)
  if (!file.exists(p)) stop("No se encontró la fuente: ", archivo)
  p
}

# ── bloque @font-face base64 (Noto Sans) ─────────────────────────────────────
face <- function(path, weight, style) sprintf(
  "@font-face{font-family:'Noto Sans';font-style:%s;font-weight:%s;src:url(data:font/ttf;base64,%s) format('truetype');}",
  style, weight, jsonlite::base64_enc(readBin(path, "raw", file.size(path)))
)
style_block <- paste0(
  "<style>\n",
  face(ttf_path("NotoSans-Regular.ttf"),                    "400", "normal"), "\n",
  face(ttf_path("NotoSans-Bold.ttf",   weight = "bold"),    "700", "normal"), "\n",
  face(ttf_path("NotoSans-Italic.ttf", style  = "italic"),  "400", "italic"), "\n",
  "</style>"
)
inyectar_fuente <- function(svg) {
  if (grepl("<!--FONT-FACE-->", svg, fixed = TRUE))
    sub("<!--FONT-FACE-->", style_block, svg, fixed = TRUE)
  else
    sub("(<defs[^>]*>)", paste0("\\1\n", style_block), svg)
}
esc <- function(s) gsub("<", "&lt;", gsub("&", "&amp;", s))

master <- paste(readLines(svg_master, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# ── variante 1: ANCHO (Word / reporte) ───────────────────────────────────────
tmp1 <- tempfile(fileext = ".svg")
writeLines(inyectar_fuente(master), tmp1, useBytes = TRUE)
png1 <- file.path(dir_out, "diagrama_canasta_digna_2026m07.png")
rsvg::rsvg_png(tmp1, png1, width = ancho_px)
unlink(tmp1)

# ── variante 2: LÁMINA (16:9) ────────────────────────────────────────────────
# Se extrae el contenido del master (sin su etiqueta <svg>) y se envuelve, escalado
# y centrado, en un lienzo 1920x1080; la fuente se incrusta UNA vez en la raíz.
inner <- master
inner <- sub("^\\s*<\\?xml[^>]*\\?>\\s*", "", inner)  # quita declaración XML
inner <- sub("^\\s*<svg[^>]*>", "", inner)            # quita <svg ...> de apertura
inner <- sub("</svg>\\s*$", "", inner)                # quita cierre </svg>
inner <- sub("<!--FONT-FACE-->", "", inner, fixed = TRUE)  # la fuente va en la raíz

# escala del flujo (viewBox 1750 de ancho) a 1800 px con márgenes de 60
esc_flujo <- 1800 / 1750

lamina <- paste0(
  '<?xml version="1.0" encoding="UTF-8"?>\n',
  '<svg xmlns="http://www.w3.org/2000/svg" width="33.87cm" height="19.05cm" ',
  'viewBox="0 0 1920 1080" font-family="Noto Sans, sans-serif">\n',
  '  <defs>', style_block, '</defs>\n',
  '  <text x="64" y="118" font-size="30" font-weight="700" letter-spacing="4" fill="#9B2247">',
  esc(lam_antetitulo), '</text>\n',
  '  <text x="64" y="182" font-size="54" font-weight="700" fill="#161A1D">',
  esc(lam_titulo), '</text>\n',
  '  <line x1="64" y1="212" x2="520" y2="212" stroke="#611232" stroke-width="4" stroke-linecap="round"/>\n',
  '  <g transform="translate(60,320) scale(', format(esc_flujo, digits = 7), ')">\n',
  inner, '\n  </g>\n',
  '  <text x="64" y="900" font-size="25" font-style="italic" fill="#6B6863">',
  esc(lam_fuente), '</text>\n',
  '</svg>\n'
)

svg2 <- file.path(dir_out, "diagrama_canasta_digna_lamina_2026m07.svg")
writeLines(lamina, svg2, useBytes = TRUE)
png2 <- file.path(dir_out, "diagrama_canasta_digna_lamina_2026m07.png")
rsvg::rsvg_png(svg2, png2, width = lamina_px)

message("PNG ancho:  ", png1, "  (", file.info(png1)$size, " bytes)")
message("SVG lámina: ", svg2, "  (", file.info(svg2)$size, " bytes)")
message("PNG lámina: ", png2, "  (", file.info(png2)$size, " bytes)")
