#_______________________________________________________________________________
# 1b_pairing_api_ccif.R
#
# Coordinación de Análisis de la Economía Laboral (CAEL) — Conasami
# Salario Digno — emparejamiento Canasta Alimentaria (ENIGH) ↔ INPC (CCIF/API)
#
# Propósito: emparejar cada producto de la canasta alimentaria (clave ENIGH) con
#   la serie del INPC que servirá para deflactar, usando la Clasificación del
#   consumo individual por finalidades (CCIF) —la misma taxonomía que estructura
#   los productos de la ENIGH—. Para cada producto se identifica el genérico CCIF
#   más fino, su ruta jerárquica y el ID de serie del BIE/API (clave de descarga).
#
# Inputs:
#   data/INP_INP20260702130421.CSV          (INPC mensual, nacional, base 2Q-jul-2018,
#                                            CCIF 2018; ancho, codificación CP1252)
#   finaldata/alimentos/12.productos_canasta.csv
#
# Outputs (finaldata/alimentos/):
#   13.catalogo_ccif_generico.csv   catálogo de genéricos CCIF (codigo, nombre, ruta, serie_api)
#   13.api_ccif_rural.csv           emparejamiento de la canasta rural
#   13.api_ccif_urbano.csv          emparejamiento de la canasta urbana
#
# Notas:
#   - El CSV de precios es SOLO NACIONAL: el ID de serie de un producto es el mismo
#     para Rural y Urbano; los dos archivos difieren solo en qué claves contiene
#     cada canasta.
#   - La estructura del CSV: línea 6 ("Título") = ruta CCIF por columna; línea 467
#     ("Fecha") = IDs de serie del BIE/API por columna, alineados por posición.
#   - El crosswalk clave_enigh -> genérico se define explícitamente abajo. Las claves
#     sin emparejamiento obvio quedan con genérico/serie vacíos y `revisar = "sí"`;
#     la columna `nota_match` guarda la propuesta tentativa para que el usuario decida
#     (no se inventan emparejamientos en las columnas de mapeo).
#_______________________________________________________________________________

rm(list = ls()); gc()
options(scipen = 999)

if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  tidyverse,
  data.table
)

# ── 1. Parseo del catálogo CCIF del CSV de precios ───────────────────────────
# Se leen solo las líneas 6 (Título) y 467 (IDs de serie), alineadas por posición.

csv_precios <- "data/INP_INP20260702130421.CSV"
lineas      <- readLines(csv_precios, encoding = "latin1", warn = FALSE)

titulo <- scan(text = lineas[6],   what = "character", sep = ",",
               quote = "\"", quiet = TRUE)
ids    <- scan(text = lineas[467], what = "character", sep = ",",
               quote = "\"", quiet = TRUE)

stopifnot(length(titulo) == length(ids))

# La columna 2 ("Total") lleva el prefijo institucional completo; se usa como
# "boilerplate" a recortar para dejar solo la ruta CCIF (de "01 ..." en adelante).
boiler <- titulo[2]

quitar_boiler <- function(t) {
  r <- ifelse(startsWith(t, boiler), substring(t, nchar(boiler) + 1L), t)
  str_trim(str_remove(r, "^,\\s*"))
}

ruta <- vapply(titulo, quitar_boiler, character(1), USE.NAMES = FALSE)

# Hoja de cada columna = último nivel CCIF "<código> Nombre". El prefijo greedy `.*`
# fuerza a capturar el ÚLTIMO límite ", <código> " (código = genérico "NNN" de 3
# dígitos, o nivel superior con puntos, p. ej. "11.1" grupo). El `.+$` captura nombres
# con comas (p. ej. "012 Pasteles, pastelillos y pan dulce empaquetado").
leaf <- str_match(ruta, "^.*(?:^|, )([0-9][0-9.]*) (.+)$")

catalogo_full <- tibble(
  codigo    = leaf[, 2],
  nombre    = leaf[, 3],
  ruta_ccif = ruta,
  serie_api = ids
) |>
  filter(!is.na(codigo)) |>
  distinct(codigo, .keep_all = TRUE)

# Catálogo de referencia: solo genéricos (código de 3 dígitos, 001–292).
catalogo_gen <- catalogo_full |>
  filter(str_detect(codigo, "^[0-9]{3}$"))

fwrite(catalogo_gen, "finaldata/alimentos/13.catalogo_ccif_generico.csv", bom = TRUE)
message("Catálogo CCIF: ", nrow(catalogo_gen), " genéricos (",
        nrow(catalogo_full), " niveles en total).")

# ── 2. Crosswalk clave_enigh -> genérico CCIF ────────────────────────────────
# Una decisión explícita por clave. `revisar = "sí"` marca los productos sin
# genérico obvio: se dejan sin código (NA) y la propuesta va en `nota_match`.

crosswalk <- tribble(
  ~clave_enigh, ~codigo_ccif, ~revisar, ~nota_match,
  # -- Cereales
  "011111", "001", "no", NA,
  "011131", "014", "no", NA,
  "011132", "015", "no", NA,
  "011133", "008", "no", "Pan (genérico) ≈ 008 Pan blanco",
  "011134", "009", "no", NA,
  "011135", "004", "no", NA,
  "011137", "010", "no", NA,
  "011150", "011", "no", NA,
  "011192", "016", "no", NA,
  # -- Carnes y embutidos
  "011221", "018", "no", NA,
  "011224", "018", "no", NA,
  "011228", "017", "no", NA,
  "01122F", "022", "no", NA,
  "011231", "021", "no", NA,
  "011252", "020", "no", NA,
  "181124", "022", "no", NA,
  "181125", "022", "no", NA,
  "181126", "019", "no", "Embutidos de aves mixtos → 019 Carnes secas, procesadas y otros embutidos (genérico paraguas de procesados)",
  "011592", "019", "no", "Chicharrón → 019 Carnes secas, procesadas y otros embutidos",
  # -- Pescados
  "011322", "026", "no", NA,
  # -- Lácteos y huevo
  "011411", "034", "no", NA,
  "011433", "030", "no", NA,
  "011451", "037", "no", NA,
  "011454", "039", "no", NA,
  "011460", "041", "no", NA,
  "011481", "031", "no", NA,
  # -- Aceites
  "011512", "042", "no", NA,
  # -- Frutas
  "011611", "045", "no", NA,
  "011616", "055", "no", NA,
  "011621", "048", "no", NA,
  "011633", "049", "no", NA,
  "181134", "055", "no", "Plátano verde/Tabasco (de cocción) ≈ 055 Plátanos",
  # -- Hortalizas y legumbres
  "011712", "071", "no", NA,
  "011721", "060", "no", NA,
  "011722", "062", "no", NA,
  "011723", "081", "no", "Chile jalapeño ≈ 081 Otros chiles frescos",
  "011725", "065", "no", NA,
  "011727", "070", "no", NA,
  "011728", "075", "no", NA,
  "011729", "076", "no", NA,
  "011743", "061", "no", NA,
  "011746", "072", "no", NA,
  "011748", "078", "no", NA,
  "011751", "073", "no", NA,
  "011761", "068", "no", NA,
  "011796", "074", "no", NA,
  "011797", "069", "no", NA,
  # -- Azúcar
  "011810", "082", "no", NA,
  # -- Condimentos
  "011942", "087", "no", NA,
  # -- Bebidas no alcohólicas
  "012100", "095", "no", NA,
  "012201", "096", "no", NA,
  "012501", "099", "no", NA,
  "012601", "100", "no", NA,
  "012602", "100", "no", NA,
  # -- Alimentos preparados / fuera del hogar
  "011911", "269", "no", "Alimentos preparados para consumo inmediato ≈ 269 Otros alimentos cocinados",
  "111111", "11.1", "no", "Desayunos (fuera de hogar) → grupo 11.1 Servicios de alimentos y bebidas (nivel grupo, engloba varios servicios)",
  "111112", "11.1", "no", "Comidas (fuera de hogar) → grupo 11.1 Servicios de alimentos y bebidas (nivel grupo, engloba varios servicios)",
  "111113", "11.1", "no", "Cenas (fuera de hogar) → grupo 11.1 Servicios de alimentos y bebidas (nivel grupo, engloba varios servicios)",
  "111124", "269", "no", NA,
  "111126", "267", "no", NA
)

# ── 3. Ensamble de la salida ─────────────────────────────────────────────────

productos <- fread("finaldata/alimentos/12.productos_canasta.csv",
                   colClasses = c(clave = "character"))

emparejado <- productos |>
  transmute(ambito, clave_enigh = clave, nombre_enigh = nombre) |>
  left_join(crosswalk, by = "clave_enigh") |>
  left_join(catalogo_full, by = c("codigo_ccif" = "codigo")) |>
  transmute(
    ambito,
    clave_enigh,
    nombre_enigh,
    ccif_generico    = if_else(is.na(codigo_ccif), NA_character_,
                               paste(codigo_ccif, nombre)),
    ccif_ruta        = ruta_ccif,
    serie_api,
    descripcion_ccif = nombre,
    revisar,
    nota_match
  ) |>
  arrange(ambito, clave_enigh)

# verificación: toda clave de la canasta debe estar en el crosswalk
faltantes <- productos |>
  distinct(clave) |>
  anti_join(crosswalk, by = c("clave" = "clave_enigh")) |>
  pull(clave)
if (length(faltantes) > 0) {
  warning("Claves de la canasta sin entrada en el crosswalk: ",
          paste(faltantes, collapse = ", "))
}

# ── 4. Escritura por ámbito ──────────────────────────────────────────────────

emparejado |>
  filter(ambito == "Rural") |>
  fwrite("finaldata/alimentos/13.api_ccif_rural.csv", bom = TRUE)

emparejado |>
  filter(ambito == "Urbano") |>
  fwrite("finaldata/alimentos/13.api_ccif_urbano.csv", bom = TRUE)

# resumen a consola
resumen <- emparejado |>
  count(ambito, revisar) |>
  pivot_wider(names_from = revisar, values_from = n, values_fill = 0)
print(resumen)

message("1b_pairing_api_ccif.R: listo. Outputs en finaldata/alimentos/ (13.*).")
