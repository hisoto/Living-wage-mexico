#_______________________________________________________________________________
# 1c_pairing_api_objetogasto.R
#
# Coordinación de Análisis de la Economía Laboral (CAEL) — Conasami
# Salario Digno — puente Canasta Alimentaria (ENIGH) ↔ INPC "por objeto del gasto"
#
# Propósito: la desagregación CCIF del INPC (script 1b) solo existe a nivel
#   nacional. La clasificación "por objeto del gasto" sí se publica desagregada
#   por entidad/ciudad, así que este script construye el catálogo de genéricos
#   "objeto del gasto" (código, nombre, ruta, ID de serie nacional) a partir de
#   la descarga nacional, y lo empareja por CÓDIGO con el pairing CCIF ya hecho
#   (código y nombre de genérico son el mismo identificador conceptual en ambas
#   vistas del INPC; solo cambia el ID de serie). El resultado es un catálogo de
#   referencia (código + nombre) para localizar, en un paso posterior, las
#   series por entidad en el catálogo/API del INEGI — este script NO las trae.
#
# Inputs:
#   data/INP_INP20260702142329.CSV          (INPC mensual, nacional, "por objeto
#                                            del gasto" + SCIAN + durabilidad;
#                                            ancho, codificación CP1252/latin1)
#   finaldata/alimentos/13.api_ccif_rural.csv
#   finaldata/alimentos/13.api_ccif_urbano.csv
#
# Outputs (finaldata/alimentos/):
#   14.catalogo_objeto_gasto_generico.csv   catálogo de genéricos "objeto del
#                                            gasto" (codigo, nombre, ruta, serie_api)
#   14.api_objeto_gasto_rural.csv           puente de la canasta rural
#   14.api_objeto_gasto_urbano.csv          puente de la canasta urbana
#
# Notas:
#   - El CSV mezcla tres clasificaciones como bloques de columnas contiguos,
#     detectables por el patrón ", por <clasificación>," en la fila 6 ("Título"):
#     "objeto del gasto", "origen de los bienes (SCIAN 2018)" y "durabilidad de
#     los bienes". Solo se usa el bloque "objeto del gasto"; el límite se ubica
#     dinámicamente (no se hardcodea el número de columna) para ser robusto a
#     re-descargas del archivo.
#   - Igual que en 1b: línea 6 ("Título") = ruta jerárquica por columna; línea
#     587 ("Fecha") = IDs de serie del BIE/API por columna, alineadas por
#     posición.
#   - El emparejamiento se hace por código (extraído de `ccif_generico` en los
#     13.api_ccif_*.csv), no reconstruyendo un crosswalk clave_enigh -> código.
#   - Los 292 genéricos (código de 3 dígitos) del catálogo CCIF tienen
#     contraparte exacta (mismo código y nombre) en "objeto del gasto"; se
#     verificó explícitamente (0 discrepancias). La única excepción es el
#     código de GRUPO "11.1" (usado en el crosswalk CCIF para comidas fuera del
#     hogar), que en "objeto del gasto" no tiene nodo equivalente: esa rama cae
#     bajo el grupo "25 Alimentos cocinados fuera de casa" en una posición
#     jerárquica distinta, sin desglose desayuno/comida/cena. Esa fila queda con
#     serie_api_nacional = NA y revisar = "sí"; no se inventa el dato.
#_______________________________________________________________________________

rm(list = ls()); gc()
options(scipen = 999)

if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  tidyverse,
  data.table
)

# ── 1. Parseo del catálogo "objeto del gasto" del CSV de precios ────────────
# Se leen solo las líneas 6 (Título) y 587 (IDs de serie), alineadas por
# posición, y se recorta al bloque de columnas "por objeto del gasto".

csv_precios <- "data/INP_INP20260702142329.CSV"
lineas      <- readLines(csv_precios, encoding = "latin1", warn = FALSE)

titulo_todo <- scan(text = lineas[6],   what = "character", sep = ",",
                    quote = "\"", quiet = TRUE)
ids_todo    <- scan(text = lineas[587], what = "character", sep = ",",
                    quote = "\"", quiet = TRUE)

stopifnot(length(titulo_todo) == length(ids_todo))

# Clasificación de cada columna (col. 1 = etiqueta "Título", se excluye).
clasificacion <- str_match(titulo_todo[-1], ", por ([^,]+),")[, 2]
bloque_og     <- which(clasificacion == "objeto del gasto") + 1L  # +1: offset por columna 1

titulo <- titulo_todo[c(1, bloque_og)]
ids    <- ids_todo[c(1, bloque_og)]

message("Bloque 'objeto del gasto': columnas ", min(bloque_og), " a ", max(bloque_og),
        " (", length(bloque_og), " series) de ", length(titulo_todo), " totales.")

# La columna 2 ("Total") lleva el prefijo institucional completo; se usa como
# "boilerplate" a recortar para dejar solo la ruta "objeto del gasto".
boiler <- titulo[2]

quitar_boiler <- function(t) {
  r <- ifelse(startsWith(t, boiler), substring(t, nchar(boiler) + 1L), t)
  str_trim(str_remove(r, "^,\\s*"))
}

ruta <- vapply(titulo, quitar_boiler, character(1), USE.NAMES = FALSE)

# Hoja de cada columna = último nivel "<código> Nombre" (mismo patrón que CCIF:
# el prefijo greedy `.*` fuerza a capturar el ÚLTIMO límite ", <código> ").
leaf <- str_match(ruta, "^.*(?:^|, )([0-9][0-9.]*) (.+)$")

catalogo_full <- tibble(
  codigo       = leaf[, 2],
  nombre_og    = leaf[, 3],
  ruta_og      = ruta,
  serie_api_og = ids
) |>
  filter(!is.na(codigo)) |>
  distinct(codigo, .keep_all = TRUE)

# Catálogo de referencia: solo genéricos (código de 3 dígitos).
catalogo_gen <- catalogo_full |>
  filter(str_detect(codigo, "^[0-9]{3}$"))

fwrite(catalogo_gen |> rename(nombre = nombre_og, serie_api = serie_api_og),
       "finaldata/alimentos/14.catalogo_objeto_gasto_generico.csv", bom = TRUE)
message("Catálogo 'objeto del gasto': ", nrow(catalogo_gen), " genéricos (",
        nrow(catalogo_full), " niveles en total).")

# ── 2. Emparejamiento por código con el pairing CCIF ya construido ──────────

notas_faltantes <- tribble(
  ~codigo,  ~nota_faltante,
  "11.1", "sin código de grupo equivalente en objeto del gasto; candidato más cercano: grupo 25 'Alimentos cocinados fuera de casa' (genéricos 263/264/266/267/269), sin desglose desayuno/comida/cena."
)

emparejar_ambito <- function(ambito_csv) {
  ccif <- fread(ambito_csv, colClasses = "character", encoding = "UTF-8")

  ccif |>
    mutate(codigo_ccif = str_extract(ccif_generico, "^\\S+")) |>
    left_join(catalogo_full, by = c("codigo_ccif" = "codigo")) |>
    left_join(notas_faltantes, by = c("codigo_ccif" = "codigo")) |>
    transmute(
      ambito,
      clave_enigh,
      nombre_enigh,
      codigo_ccif,
      objeto_gasto_generico = if_else(is.na(serie_api_og), NA_character_,
                                       paste(codigo_ccif, nombre_og)),
      objeto_gasto_ruta     = ruta_og,
      serie_api_nacional    = serie_api_og,
      revisar    = if_else(is.na(serie_api_og) | revisar == "sí", "sí", "no"),
      nota_match = case_when(
        !is.na(nota_faltante) & !is.na(nota_match) ~ paste(nota_match, "|", nota_faltante),
        !is.na(nota_faltante)                       ~ nota_faltante,
        TRUE                                         ~ nota_match
      )
    )
}

emparejado <- bind_rows(
  emparejar_ambito("finaldata/alimentos/13.api_ccif_rural.csv"),
  emparejar_ambito("finaldata/alimentos/13.api_ccif_urbano.csv")
) |>
  arrange(ambito, clave_enigh)

# ── 3. Escritura por ámbito ──────────────────────────────────────────────────

emparejado |>
  filter(ambito == "Rural") |>
  fwrite("finaldata/alimentos/14.api_objeto_gasto_rural.csv", bom = TRUE)

emparejado |>
  filter(ambito == "Urbano") |>
  fwrite("finaldata/alimentos/14.api_objeto_gasto_urbano.csv", bom = TRUE)

# resumen a consola
resumen <- emparejado |>
  count(ambito, revisar) |>
  pivot_wider(names_from = revisar, values_from = n, values_fill = 0)
print(resumen)

message("1c_pairing_api_objetogasto.R: listo. Outputs en finaldata/alimentos/ (14.*).")
