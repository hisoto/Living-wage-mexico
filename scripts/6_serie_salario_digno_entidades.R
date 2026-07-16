#_______________________________________________________________________________
# 6_serie_salario_digno_entidades.R
#
# Coordinación de Análisis de la Economía Laboral (CAEL) — Conasami
# Salario Digno — SERIE TEMPORAL MENSUAL del valor por ENTIDAD (32 × ámbito)
#
# Propósito: análogo subnacional del script 5. El pipeline 1→4 produce el valor
#   PUNTUAL del salario digno por entidad×ámbito (salario_digno.csv), anclado al
#   nivel de precios de la ENIGH 2024. Este script lo convierte en una SERIE mensual
#   por entidad, deflactando los tres componentes con índices del INPC POR ENTIDAD
#   (clasificación "objeto del gasto", la única desagregada subnacionalmente),
#   anclados en agosto 2024.
#
# Método (por entidad e, ámbito a, mes t) — mismo que el script 5 pero con series
#   estatales "objeto del gasto" en lugar de CCIF nacional:
#   Alimentos: alim = 1.20 * Σ_clave valor_base[e,a,clave]
#                     * idx[e, codigo(clave), t] / idx[e, codigo(clave), ago-2024]
#              (Laspeyres: cantidades ENIGH 2024 fijas; cada producto se reajusta con
#               la serie estatal de su genérico "objeto del gasto"). Las comidas fuera
#               del hogar (111111/111112/111113) no tienen genérico equivalente en
#               objeto del gasto → se deflactan con el grupo 25 "Alimentos cocinados
#               fuera de casa" de la entidad (decisión del usuario).
#   Vivienda:  viv  = viv_base[e,a]  * idx_viv[e,t]  / idx_viv[e,ago-2024]
#              (división "3. Vivienda" de la entidad).
#   NANV:      nanv = nanv_base[e,a] * idx_gen[e,t] / idx_gen[e,ago-2024]
#              (Índice general de la entidad).
#   Salario digno = ((alim*4) + viv + (nanv*4)) * 1.05 / 2.
#   En t = ago-2024 la serie reproduce salario_digno.csv por entidad.
#
# Fuente de códigos de serie: los 32 CSV de data/entidades_08_07_2026/ traen, por
#   entidad, la tabla "objeto del gasto" con la ruta de cada nodo (fila "Título") y
#   su ID de serie BIE/API (fila "Fecha"). Se usan como CATÁLOGO offline. Los VALORES
#   se obtienen por scraping del portal INEGI (esos CSV solo exportan los últimos ~17
#   meses, sin el ancla ago-2024).
#
# Inputs:
#   data/entidades_08_07_2026/*.CSV                       (catálogo códigos por entidad)
#   finaldata/alimentos/6.precios_ca_{rural,urbano}.csv   (base alimentos por entidad)
#   finaldata/alimentos/14.api_objeto_gasto_{rural,urbano}.csv (clave_enigh -> codigo)
#   salario_digno.csv                                     (base vivienda y NANV, cve_ent)
#   scripts/theme_conasami_dt2026.R, scripts/utils_inpc_scraper.R
#
# Outputs:
#   finaldata/serie/catalogo_series_entidades.csv            (código<->serie por entidad)
#   finaldata/serie/pairing_alimentos_entidades_{rural,urbano}.csv (serie por producto)
#   finaldata/serie/serie_salario_digno_entidades.csv        (serie + componentes)
#   finaldata/serie/serie_componentes_entidades.csv          (componentes, formato largo)
#   data/inpc_series_cache_entidades.csv                     (caché crudo de índices)
#   graphs/serie/entidades/{Entidad}_serie.{png,svg}         (una gráfica por entidad)
#
# Dependencias: requiere el pipeline 1→4 corrido e internet al primer run (luego caché).
#_______________________________________________________________________________

rm(list = ls()); gc()
options(scipen = 999)

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, rvest, httr2, xml2, data.table, here, glue, zoo)

source(here::here("scripts", "theme_conasami_dt2026.R"))
source(here::here("scripts", "utils_inpc_scraper.R"))   # get_inpc_serie()

# ── parámetros ────────────────────────────────────────────────────────────────
anio_ini        <- 2016
anio_fin        <- as.integer(format(Sys.Date(), "%Y"))
mes_base        <- as.Date("2024-08-01")   # ancla ENIGH 2024 (deflactor = 1)
forzar_descarga <- FALSE                    # TRUE re-scrapea aunque exista el caché

# Inicio efectivo de la serie. Las series "objeto del gasto" por entidad arrancan en
# jul-2018 (base 2Q-jul-2018 = 100). Se recorta a ago-2018 para empatar con el inicio
# de la serie nacional (script 5).
fecha_inicio_serie <- as.Date("2018-08-01")

dir_ent    <- here::here("data", "entidades_08_07_2026")
dir_serie  <- here::here("finaldata", "serie")
dir_graphs <- here::here("graphs", "serie", "entidades")
cache_path <- here::here("data", "inpc_series_cache_entidades.csv")
if (!dir.exists(dir_serie))  dir.create(dir_serie,  recursive = TRUE)
if (!dir.exists(dir_graphs)) dir.create(dir_graphs, recursive = TRUE)

meses_es <- c(Ene = 1, Feb = 2, Mar = 3, Abr = 4, May = 5, Jun = 6,
              Jul = 7, Ago = 8, Sep = 9, Oct = 10, Nov = 11, Dic = 12)

# ── 1. catálogo de series por entidad (offline, de los 32 CSV) ────────────────
# Cada CSV: fila "Título" = ruta de cada nodo (clasificación "objeto del gasto");
# fila "Fecha" = ID de serie BIE/API por columna, alineado por posición. Se parsea
# igual que scripts/1c (localizando las filas por su etiqueta, robusto a re-descargas).

parse_entidad <- function(f) {
  lineas <- readLines(f, encoding = "latin1", warn = FALSE)
  lineas <- iconv(lineas, from = "latin1", to = "UTF-8")

  idx_t <- which(str_detect(lineas, '^"?T[íi]tulo"?,'))[1]   # [íi]: evita el acento
  idx_f <- which(str_detect(lineas, '^"?Fecha"?,'))[1]
  stopifnot(!is.na(idx_t), !is.na(idx_f))

  titulo <- scan(text = lineas[idx_t], what = "character", sep = ",",
                 quote = "\"", quiet = TRUE)
  ids    <- scan(text = lineas[idx_f], what = "character", sep = ",",
                 quote = "\"", quiet = TRUE)
  stopifnot(length(titulo) == length(ids))

  # hoja "<codigo> Nombre" del final de la ruta (mismo patrón que 1b/1c). El general
  # no tiene código numérico (queda NA y se clasifica por la ruta más abajo).
  leaf <- str_match(titulo, "^.*(?:^|, )([0-9][0-9.]*) (.+)$")

  tibble(ruta = titulo, codigo = leaf[, 2], nombre = leaf[, 3], serie = ids) |>
    slice(-1) |>                                 # quita la columna 1 (etiqueta)
    mutate(componente = case_when(
      str_detect(ruta, "objeto del gasto, Índice general$")     ~ "nanv",
      str_detect(ruta, ", 3\\. Vivienda$")                      ~ "vivienda",
      str_detect(ruta, ", 25 Alimentos cocinados fuera de casa$") ~ "comidas_fuera",
      str_detect(codigo, "^[0-9]{3}$")                          ~ "generico",
      TRUE                                                      ~ NA_character_
    ))
}

archivos_ent <- list.files(dir_ent, pattern = "\\.CSV$", full.names = TRUE)
stopifnot(length(archivos_ent) == 32)

cat_ent <- map_dfr(archivos_ent, function(f) {
  nom <- str_remove(basename(f), " \\(mensual\\)_08_07_2026\\.CSV$")
  parse_entidad(f) |> mutate(nom_ent = nom)
})

# nom_ent -> cve_ent desde salario_digno.csv (fuente de verdad de claves)
sd <- fread(here::here("salario_digno.csv"))
cve_lookup <- sd |> distinct(nom_ent, cve_ent)

falt_cve <- setdiff(unique(cat_ent$nom_ent), cve_lookup$nom_ent)
if (length(falt_cve) > 0)
  stop("Entidades sin cve_ent (revisar nombres de archivo): ",
       paste(falt_cve, collapse = ", "))

# nodos especiales: deben existir exactamente una vez por entidad
chk_unico <- cat_ent |>
  filter(componente %in% c("nanv", "vivienda", "comidas_fuera")) |>
  count(nom_ent, componente) |>
  filter(n != 1)
if (nrow(chk_unico) > 0) {
  print(chk_unico)
  stop("Algún nodo especial (general/vivienda/grupo25) no aparece exactamente 1 vez por entidad.")
}

# claves de la canasta y su código "objeto del gasto" (clave_enigh -> codigo_ccif)
og <- bind_rows(
  fread(here::here("finaldata", "alimentos", "14.api_objeto_gasto_rural.csv"),
        colClasses = "character"),
  fread(here::here("finaldata", "alimentos", "14.api_objeto_gasto_urbano.csv"),
        colClasses = "character")
)

# genéricos que usa la canasta (excluye 11.1 comidas fuera, que va por grupo 25)
codigos_canasta <- og |>
  filter(codigo_ccif != "11.1") |>
  distinct(codigo_ccif) |>
  pull(codigo_ccif)

generics <- cat_ent |> filter(componente == "generico") |>
  select(nom_ent, codigo, nombre_nodo = nombre, serie_api = serie)

# cobertura: cada código de la canasta debe existir en cada entidad
cob <- crossing(nom_ent = unique(cat_ent$nom_ent), codigo = codigos_canasta) |>
  left_join(generics, by = c("nom_ent", "codigo")) |>
  filter(is.na(serie_api))
if (nrow(cob) > 0) {
  print(head(cob, 20))
  stop("Hay códigos de la canasta sin serie en alguna entidad (", nrow(cob), " casos).")
}

# catálogo largo (driver del scraping + entregable de códigos por entidad)
catalogo <- bind_rows(
  generics |> filter(codigo %in% codigos_canasta) |>
    mutate(componente = "alimentos"),
  cat_ent |> filter(componente == "comidas_fuera") |>
    transmute(nom_ent, codigo = "25", nombre_nodo = nombre, serie_api = serie,
              componente = "comidas_fuera"),
  cat_ent |> filter(componente == "vivienda") |>
    transmute(nom_ent, codigo = "3", nombre_nodo = "Vivienda", serie_api = serie,
              componente = "vivienda"),
  cat_ent |> filter(componente == "nanv") |>
    transmute(nom_ent, codigo = "general", nombre_nodo = "Índice general",
              serie_api = serie, componente = "nanv")
) |>
  left_join(cve_lookup, by = "nom_ent") |>
  select(nom_ent, cve_ent, componente, codigo, nombre_nodo, serie_api) |>
  arrange(cve_ent, componente, codigo)

fwrite(catalogo, file.path(dir_serie, "catalogo_series_entidades.csv"), bom = TRUE)
message("Catálogo por entidad: ", nrow(catalogo), " filas (",
        n_distinct(catalogo$serie_api), " series distintas).")

# ── 2. pairing por producto y ámbito (clave_enigh -> serie estatal) ───────────
# Une la canasta (clave_enigh -> codigo_ccif, por ámbito) con las series estatales.
# Comidas fuera del hogar (codigo_ccif == "11.1") -> serie de grupo 25 de la entidad.

grupo25 <- cat_ent |> filter(componente == "comidas_fuera") |>
  select(nom_ent, serie25 = serie)

# solo las 32 entidades parseadas (excluye "Nacional", que es del script 5)
entidades <- cat_ent |> distinct(nom_ent) |>
  left_join(cve_lookup, by = "nom_ent") |> arrange(cve_ent)

pair_ambito <- function(amb) {
  ogx <- og |> filter(ambito == amb) |>
    select(clave_enigh, nombre_enigh, codigo_ccif)

  crossing(entidades, ogx) |>
    left_join(generics, by = c("nom_ent", "codigo_ccif" = "codigo")) |>
    left_join(grupo25,  by = "nom_ent") |>
    mutate(
      es_comida_fuera = codigo_ccif == "11.1",
      codigo    = if_else(es_comida_fuera, "25", codigo_ccif),
      serie_api = if_else(es_comida_fuera, serie25, serie_api),
      ambito    = amb
    ) |>
    select(nom_ent, cve_ent, ambito, clave_enigh, nombre_enigh,
           codigo, serie_api, es_comida_fuera) |>
    arrange(cve_ent, clave_enigh)
}

pair_rural  <- pair_ambito("Rural")
pair_urbano <- pair_ambito("Urbano")

# toda fila debe tener serie
stopifnot(!anyNA(pair_rural$serie_api), !anyNA(pair_urbano$serie_api))

fwrite(pair_rural,  file.path(dir_serie, "pairing_alimentos_entidades_rural.csv"),  bom = TRUE)
fwrite(pair_urbano, file.path(dir_serie, "pairing_alimentos_entidades_urbano.csv"), bom = TRUE)
message("Pairing alimentos: rural ", nrow(pair_rural),
        " filas, urbano ", nrow(pair_urbano), " filas.")

# ── 3. descarga de índices por scraping (con caché incremental) ───────────────
# Todas las series estatales "objeto del gasto" viven en UNA estructura del portal:
# "por entidad federativa" (base 2Q-jul-2018, canasta 2024). Se localizó navegando
# el árbol raíz idEstructura=1120 -> base 11200170 -> sección 007 (los 32 nodos
# "NN Entidad" comparten los 12 primeros dígitos = 112001700070). Un solo
# idEstructura basta para las ~1,700 series; se piden por lotes (coma-separadas).

id_entidad_og <- "112001700070"   # "por entidad federativa", base 2Q-jul-2018/2024
tam_lote      <- 50               # series por consulta

series_all <- sort(unique(catalogo$serie_api))

fetch_lote <- function(ids, intento = 1L) {
  tryCatch(
    get_inpc_series_lote(id_entidad_og, ids, anio_ini = 2018, anio_fin = anio_fin),
    error = function(e) {
      if (intento < 3L) { Sys.sleep(3); fetch_lote(ids, intento + 1L) }
      else stop("Lote falló tras 3 intentos: ", conditionMessage(e))
    }
  )
}

parse_periodo <- function(df) {
  df |>
    mutate(
      mes_txt = str_extract(periodo, "^[A-Za-z]+"),
      year    = as.integer(str_extract(periodo, "\\d{4}")),
      month   = meses_es[mes_txt],
      date    = as.Date(sprintf("%04d-%02d-01", year, month)),
      valor   = as.double(valor)
    ) |>
    select(serie, year, month, date, valor)
}

if (forzar_descarga && file.exists(cache_path)) file.remove(cache_path)

# resume: solo se bajan las series que aún no están en el caché
cacheadas <- if (file.exists(cache_path))
  unique(fread(cache_path, colClasses = list(character = "serie"))$serie) else character(0)
pendientes <- setdiff(series_all, cacheadas)

if (length(pendientes) > 0) {
  message("Descargando ", length(pendientes), " series en lotes de ", tam_lote,
          " (idEstructura ", id_entidad_og, ")...")
  lotes <- split(pendientes, ceiling(seq_along(pendientes) / tam_lote))
  for (i in seq_along(lotes)) {
    message("  lote ", i, "/", length(lotes), " (", length(lotes[[i]]), " series)")
    Sys.sleep(0.4)  # cortesía con el servidor
    lote <- fetch_lote(lotes[[i]]) |> parse_periodo()
    fwrite(lote, cache_path, bom = TRUE, append = file.exists(cache_path))
  }
  message("Caché guardado/actualizado: ", cache_path)
} else {
  message("Todas las series ya están en el caché: ", cache_path,
          " (forzar_descarga = TRUE para re-descargar).")
}

inpc <- fread(cache_path, colClasses = list(character = "serie"))
inpc[, date := as.Date(date)]

# cobertura: toda serie requerida debe estar en el caché
faltan_serie <- setdiff(series_all, unique(inpc$serie))
if (length(faltan_serie) > 0)
  stop("Series sin datos tras el scraping (", length(faltan_serie), "): ",
       paste(head(faltan_serie, 10), collapse = ", "))

# ── 4. deflactores base agosto 2024 (idx_t / idx_ago2024) ─────────────────────

base_idx <- inpc |> filter(date == mes_base) |> select(serie, idx_base = valor)

faltan_base <- setdiff(unique(inpc$serie), base_idx$serie)
if (length(faltan_base) > 0)
  warning("Series sin dato en ", mes_base, " (", length(faltan_base),
          "): ", paste(head(faltan_base, 10), collapse = ", "))

deflactores <- inpc |>
  inner_join(base_idx, by = "serie") |>
  filter(date >= fecha_inicio_serie) |>
  mutate(deflactor = valor / idx_base) |>
  select(serie, date, deflactor)

fechas <- sort(unique(deflactores$date))

# ── 5. componentes por entidad × ámbito × mes ─────────────────────────────────

# 5.1 Alimentos: base por (ámbito, entidad, clave) * deflactor de su serie estatal.
base_alim <- bind_rows(
  fread(here::here("finaldata", "alimentos", "6.precios_ca_rural.csv"),
        colClasses = list(character = "clave"))[nom_ent != "Nacional"][, ambito := "Rural"],
  fread(here::here("finaldata", "alimentos", "6.precios_ca_urbano.csv"),
        colClasses = list(character = "clave"))[nom_ent != "Nacional"][, ambito := "Urbano"]
) |>
  select(ambito, nom_ent, clave, valor_base = valor)

pairing <- bind_rows(pair_rural, pair_urbano) |>
  select(ambito, nom_ent, cve_ent, clave_enigh, serie_api)

# malla producto × fecha, con arrastre (LOCF) por si alguna serie no cubre todo el
# rango, para no sesgar el nivel al perder productos del sumatorio (cf. script 5 §6.1).
alim_serie <- pairing |>
  inner_join(base_alim, by = c("ambito", "nom_ent", "clave_enigh" = "clave")) |>
  crossing(date = fechas) |>
  left_join(deflactores, by = c("serie_api" = "serie", "date")) |>
  arrange(ambito, nom_ent, clave_enigh, date) |>
  group_by(ambito, nom_ent, clave_enigh) |>
  mutate(deflactor = na.locf(na.locf(deflactor, na.rm = FALSE), fromLast = TRUE,
                             na.rm = FALSE)) |>
  ungroup() |>
  group_by(ambito, nom_ent, cve_ent, date) |>
  summarise(canasta_alimentos = 1.20 * sum(valor_base * deflactor), .groups = "drop")

# 5.2 Vivienda (división 3) y NANV (Índice general): base por entidad×ámbito *
# deflactor de la serie estatal correspondiente (serie común a ambos ámbitos).
base_vn <- fread(here::here("salario_digno.csv")) |>
  filter(nom_ent != "Nacional") |>
  transmute(ambito, nom_ent, cve_ent, vivienda_base = vivienda_mensual, nanv_base = NANV)

serie_viv <- catalogo |> filter(componente == "vivienda") |> select(nom_ent, serie_viv = serie_api)
serie_gen <- catalogo |> filter(componente == "nanv")     |> select(nom_ent, serie_gen = serie_api)

# (many-to-many esperado: base_vn trae 2 ámbitos por entidad y la serie estatal de
#  vivienda/general es común a ambos, replicándose sobre todas las fechas.)
viv_serie <- base_vn |>
  inner_join(serie_viv, by = "nom_ent") |>
  inner_join(deflactores, by = c("serie_viv" = "serie"),
             relationship = "many-to-many") |>
  transmute(ambito, nom_ent, cve_ent, date, vivienda_mensual = vivienda_base * deflactor)

nanv_serie <- base_vn |>
  inner_join(serie_gen, by = "nom_ent") |>
  inner_join(deflactores, by = c("serie_gen" = "serie"),
             relationship = "many-to-many") |>
  transmute(ambito, nom_ent, cve_ent, date, NANV = nanv_base * deflactor)

# ── 6. ensamble del salario digno y salidas ───────────────────────────────────

serie <- alim_serie |>
  inner_join(viv_serie,  by = c("ambito", "nom_ent", "cve_ent", "date")) |>
  inner_join(nanv_serie, by = c("ambito", "nom_ent", "cve_ent", "date")) |>
  mutate(
    rural                  = as.integer(ambito == "Rural"),
    alimentos_familiar     = canasta_alimentos * 4,
    nanv_familiar          = NANV * 4,
    ingreso_digno_familiar = (alimentos_familiar + vivienda_mensual + nanv_familiar) * 1.05,
    salario_digno          = ingreso_digno_familiar / 2
  ) |>
  select(nom_ent, cve_ent, ambito, rural, date, canasta_alimentos, vivienda_mensual, NANV,
         alimentos_familiar, nanv_familiar, ingreso_digno_familiar, salario_digno) |>
  arrange(cve_ent, ambito, date)

fwrite(serie, file.path(dir_serie, "serie_salario_digno_entidades.csv"), bom = TRUE)

componentes <- serie |>
  select(nom_ent, cve_ent, ambito, date, Alimentos = canasta_alimentos,
         Vivienda = vivienda_mensual, NANV) |>
  pivot_longer(c(Alimentos, Vivienda, NANV), names_to = "componente", values_to = "valor")

fwrite(componentes, file.path(dir_serie, "serie_componentes_entidades.csv"), bom = TRUE)

# chequeo de anclaje: en ago-2024 la serie debe reproducir salario_digno.csv
chk <- serie |>
  filter(date == mes_base) |>
  select(nom_ent, ambito, salario_digno_serie = salario_digno) |>
  left_join(
    fread(here::here("salario_digno.csv"))[nom_ent != "Nacional",
      .(nom_ent, ambito, salario_digno_puntual = salario_digno)],
    by = c("nom_ent", "ambito")
  ) |>
  mutate(dif = salario_digno_serie - salario_digno_puntual)
message("Chequeo de anclaje (ago-2024): max |dif| = ",
        formatC(max(abs(chk$dif)), format = "e", digits = 2),
        " sobre ", nrow(chk), " filas (entidad×ámbito).")
if (max(abs(chk$dif)) > 1e-6) {
  print(chk |> arrange(desc(abs(dif))) |> head(10))
  warning("El anclaje no reproduce salario_digno.csv en alguna entidad (ver arriba).")
}

# ── 7. gráficas DT 2026: una por entidad (rural vs urbano) ─────────────────────
# Marca dos puntos por serie: el último mes (etiqueta arriba) y el valor base
# ago-2024 (ancla ENIGH 2024; etiqueta abajo + línea vertical punteada).

ult <- max(serie$date)
entidades_plot <- serie |> distinct(nom_ent, cve_ent) |> arrange(cve_ent)

for (i in seq_len(nrow(entidades_plot))) {
  ent  <- entidades_plot$nom_ent[i]
  d    <- serie |> filter(nom_ent == ent)
  etq  <- d |> group_by(ambito) |> filter(date == max(date)) |> ungroup()
  base <- d |> filter(date == mes_base)

  g <- ggplot(d, aes(date, salario_digno, color = ambito)) +
    geom_vline(xintercept = mes_base, linetype = "dashed", linewidth = 0.3,
               color = conasami_neutros[["eje_base"]]) +
    geom_line(linewidth = 0.75, lineend = "round", linejoin = "round") +
    geom_point(data = etq, size = 1.4) +
    geom_text(data = etq,
              aes(label = scales::label_comma(prefix = "$", accuracy = 1)(salario_digno)),
              family = "Noto Sans Tab", size = 2.8, vjust = -0.9, show.legend = FALSE) +
    geom_point(data = base, size = 1.4) +
    geom_text(data = base,
              aes(label = scales::label_comma(prefix = "$", accuracy = 1)(salario_digno)),
              family = "Noto Sans Tab", size = 2.8, vjust = 1.9, show.legend = FALSE) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year",
                 expand = expansion(mult = c(0.02, 0.08))) +
    scale_y_continuous(labels = scales::label_comma(prefix = "$"),
                       expand = expansion(mult = c(0.05, 0.10))) +
    scale_color_conasami() +
    labs(color = NULL) +
    theme_conasami()

  archivo <- paste0(str_replace_all(ent, "[^A-Za-z0-9]+", "_"), "_serie")
  guardar_grafica_conasami(g, archivo, tamano = "ancho", dir = dir_graphs)
}

message("6_serie_salario_digno_entidades.R: listo. Series en ", dir_serie,
        " y ", nrow(entidades_plot), " gráficas en ", dir_graphs, ".")
