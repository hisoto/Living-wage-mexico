#_______________________________________________________________________________
# 5_serie_salario_digno_nacional.R
#
# Coordinación de Análisis de la Economía Laboral (CAEL) — Conasami
# Salario Digno — SERIE TEMPORAL MENSUAL del valor NACIONAL
#
# Propósito: el pipeline 1→4 produce un valor PUNTUAL del salario digno anclado
#   al nivel de precios de la ENIGH 2024 (salario_digno.csv). Este script lo
#   convierte en una SERIE mensual nacional (Rural y Urbano) proyectando los tres
#   componentes con índices de precios del INPC (INEGI), anclados en agosto 2024.
#
# Método (por mes t y ámbito a):
#   Alimentos:  alim_t = 1.20 * Σ_clave [ valor_base[clave]
#                        * idx_ccif[serie(clave), t] / idx_ccif[serie(clave), ago-2024] ]
#               (Laspeyres: cantidades ENIGH 2024 fijas; cada producto se reajusta
#                por su propia serie CCIF nacional).
#   Vivienda:   viv_t  = viv_base[a]  * idx_viv[t]  / idx_viv[ago-2024]
#               (índice de la división "3. Vivienda", clasificación por objeto del gasto).
#   NANV:       nanv_t = nanv_base[a] * idx_inpc[t] / idx_inpc[ago-2024]  (INPC general).
#   Salario digno_t = ((alim_t*4) + viv_t + (nanv_t*4)) * 1.05 / 2.
#   En t = ago-2024 la serie reproduce el valor puntual de salario_digno.csv.
#
# Fuente de índices: scraping ASP.NET del portal de índices de precios del INEGI
#   (paquete inegiR caído). Método tomado de Informes/comportamiento_precios
#   (rscripts/datos_02.R). Las series CCIF y de objeto del gasto solo se publican
#   a nivel nacional, consistente con que la serie pedida es nacional.
#
# Inputs:
#   finaldata/alimentos/6.precios_ca_{rural,urbano}.csv   (valor base por clave, Nacional)
#   finaldata/alimentos/13.api_ccif_{rural,urbano}.csv    (clave_enigh -> serie_api CCIF)
#   salario_digno.csv                                     (vivienda_mensual, NANV base, Nacional)
#   data/INP_INP20260702142329.CSV                        (localiza el ID de serie de "3. Vivienda")
#   scripts/theme_conasami_dt2026.R
#
# Outputs:
#   finaldata/serie/serie_salario_digno_nacional.csv   (serie del salario digno + componentes)
#   finaldata/serie/serie_componentes_nacional.csv     (componentes en formato largo)
#   data/inpc_series_cache.csv                          (caché crudo de índices descargados)
#   graphs/serie/salario_digno_nacional_{YYYY}mMM.png (+ .svg)
#
# Dependencias: requiere haber corrido el pipeline 1→4. Requiere internet al
#   primer run (luego usa el caché). Independiente de maps.R.
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

# Inicio efectivo de la serie: primer mes con TODAS las series relevantes de la
# canasta. El INPC-CCIF no publica "Servicios de alimentos y bebidas" (909107,
# comidas fuera del hogar, 8-16% de la canasta) antes de ago-2018, así que la
# serie se recorta ahí. El único producto aún incompleto tras el corte es cilantro
# (908808, ~0.3%, arranca ago-2024); su deflactor se arrastra hacia atrás para no
# alterar la composición de la canasta (§6.1).
fecha_inicio_serie <- as.Date("2018-08-01")

# idEstructura del portal de índices de precios del INEGI
id_ccif           <- "112001700080"   # todas las series de la CCIF
id_objeto_gasto   <- "112001700030"   # INPC por objeto del gasto (incluye "3. Vivienda")
id_inpc_general   <- "112001700010"   # INPC general
serie_inpc_general <- "865541"        # INPC general nacional

# Nodo de vivienda en "objeto del gasto". "Vivienda" = división 3 (renta, energía,
# gas, agua, servicios domésticos). Alterno más estrecho: "Costo de uso de vivienda"
# (nodo 3.1). Cambiar aquí si se prefiere ese.
nombre_nodo_vivienda <- "Vivienda"

csv_objeto_gasto <- here::here("data", "INP_INP20260702142329.CSV")
cache_path       <- here::here("data", "inpc_series_cache.csv")

dir_serie  <- here::here("finaldata", "serie")
dir_graphs <- here::here("graphs", "serie")
if (!dir.exists(dir_serie))  dir.create(dir_serie,  recursive = TRUE)
if (!dir.exists(dir_graphs)) dir.create(dir_graphs, recursive = TRUE)

meses_es <- c(Ene = 1, Feb = 2, Mar = 3, Abr = 4, May = 5, Jun = 6,
              Jul = 7, Ago = 8, Sep = 9, Oct = 10, Nov = 11, Dic = 12)

# ── 1. función de scraping ────────────────────────────────────────────────────
# get_inpc_serie() vive ahora en scripts/utils_inpc_scraper.R (sourceada arriba),
# compartida con 6_serie_salario_digno_entidades.R.

# ── 2. localizar el ID de serie de la división "Vivienda" (objeto del gasto) ───
# El CSV mezcla tres clasificaciones; se detecta el bloque "objeto del gasto" por
# el patrón ", por <clasificación>," en la fila de "Título" (mismo criterio que
# scripts/1c). El ID de serie vive en la fila etiquetada "Fecha", alineada por
# posición de columna.

lineas <- readLines(csv_objeto_gasto, encoding = "latin1", warn = FALSE)

idx_titulo <- which(str_detect(lineas, '^"?T[íi]tulo"?,'))[1]
idx_fecha  <- which(str_detect(lineas, '^"?Fecha"?,'))[1]
stopifnot(!is.na(idx_titulo), !is.na(idx_fecha))

titulo_todo <- scan(text = lineas[idx_titulo], what = "character", sep = ",",
                    quote = "\"", quiet = TRUE)
ids_todo    <- scan(text = lineas[idx_fecha],  what = "character", sep = ",",
                    quote = "\"", quiet = TRUE)
stopifnot(length(titulo_todo) == length(ids_todo))

clasificacion <- str_match(titulo_todo[-1], ", por ([^,]+),")[, 2]
bloque_og     <- which(clasificacion == "objeto del gasto") + 1L  # +1: offset col. 1

titulo_og <- titulo_todo[bloque_og]
ids_og    <- ids_todo[bloque_og]

boiler <- titulo_og[1]  # col. "Total": prefijo institucional a recortar
quitar_boiler <- function(t) {
  r <- ifelse(startsWith(t, boiler), substring(t, nchar(boiler) + 1L), t)
  str_trim(str_remove(r, "^,\\s*"))
}
ruta_og <- vapply(titulo_og, quitar_boiler, character(1), USE.NAMES = FALSE)
leaf     <- str_match(ruta_og, "^.*(?:^|, )([0-9][0-9.]*) (.+)$")

viv <- tibble(codigo = leaf[, 2], nombre = leaf[, 3],
              ruta = ruta_og, serie = ids_og) |>
  filter(!is.na(codigo), nombre == nombre_nodo_vivienda) |>
  slice_min(nchar(ruta), n = 1)   # la división (ruta más corta)

stopifnot(nrow(viv) == 1)
serie_vivienda <- viv$serie
message("Serie 'objeto del gasto / ", viv$codigo, " ", viv$nombre,
        "' -> id ", serie_vivienda)

# ── 3. catálogo de series a descargar (CCIF por producto + vivienda + INPC) ────

ccif_rural  <- fread(here::here("finaldata", "alimentos", "13.api_ccif_rural.csv"),
                     colClasses = "character", encoding = "UTF-8")
ccif_urbano <- fread(here::here("finaldata", "alimentos", "13.api_ccif_urbano.csv"),
                     colClasses = "character", encoding = "UTF-8")

series_ccif <- union(ccif_rural$serie_api, ccif_urbano$serie_api)

tbl_series <- bind_rows(
  tibble(grupo = "ccif",     idEstructura = id_ccif,         serie = series_ccif),
  tibble(grupo = "vivienda", idEstructura = id_objeto_gasto, serie = serie_vivienda),
  tibble(grupo = "inpc",     idEstructura = id_inpc_general, serie = serie_inpc_general)
) |>
  distinct(serie, .keep_all = TRUE)

message("Series a descargar: ", nrow(tbl_series),
        " (", length(series_ccif), " CCIF + vivienda + INPC general)")

# ── 4. descarga (con caché) ───────────────────────────────────────────────────

if (file.exists(cache_path) && !forzar_descarga) {
  message("Usando caché de índices: ", cache_path,
          " (forzar_descarga = TRUE para re-descargar).")
  inpc <- fread(cache_path, colClasses = list(character = "serie"))
  inpc[, date := as.Date(date)]
} else {
  message("Descargando índices del INEGI (esto tarda unos minutos)...")
  inpc <- tbl_series |>
    pmap_dfr(function(grupo, idEstructura, serie) {
      Sys.sleep(0.3)  # cortesía con el servidor
      get_inpc_serie(idEstructura, paste0("e|", serie),
                     anio_ini = anio_ini, anio_fin = anio_fin) |>
        mutate(serie = serie, grupo = grupo)
    }) |>
    mutate(
      mes_txt = str_extract(periodo, "^[A-Za-z]+"),
      year    = as.integer(str_extract(periodo, "\\d{4}")),
      month   = meses_es[mes_txt],
      date    = as.Date(sprintf("%04d-%02d-01", year, month)),
      valor   = as.double(valor)
    ) |>
    select(grupo, serie, year, month, date, valor)

  fwrite(inpc, cache_path, bom = TRUE)
  message("Caché guardado: ", cache_path)
}

# ── 5. deflactores base agosto 2024 (idx_t / idx_ago2024) ─────────────────────

base_idx <- inpc |>
  filter(date == mes_base) |>
  select(serie, idx_base = valor)

deflactores <- inpc |>
  inner_join(base_idx, by = "serie") |>
  mutate(deflactor = valor / idx_base) |>
  select(grupo, serie, date, deflactor)

faltan_base <- setdiff(unique(inpc$serie), base_idx$serie)
if (length(faltan_base) > 0)
  warning("Series sin dato en ", mes_base, ": ",
          paste(faltan_base, collapse = ", "))

# ── 6. componentes nacionales por ámbito y mes ────────────────────────────────

# 6.1 Alimentos: valor base por clave (Nacional) * deflactor CCIF de su serie.
base_alim <- bind_rows(
  fread(here::here("finaldata", "alimentos", "6.precios_ca_rural.csv"),
        colClasses = list(character = "clave"))[nom_ent == "Nacional"][, ambito := "Rural"],
  fread(here::here("finaldata", "alimentos", "6.precios_ca_urbano.csv"),
        colClasses = list(character = "clave"))[nom_ent == "Nacional"][, ambito := "Urbano"]
) |>
  select(ambito, clave, valor_base = valor)

ccif_map <- bind_rows(ccif_rural, ccif_urbano) |>
  select(ambito, clave = clave_enigh, serie = serie_api)

# Malla completa producto (ambito × clave) × fecha (>= inicio). Cada producto se
# rellena por arrastre (LOCF hacia adelante y hacia atrás) para que la canasta
# tenga SIEMPRE la misma composición y ningún producto se caiga del sumatorio en
# meses sin dato propio (evita sesgar el nivel a la baja). Tras el recorte a
# ago-2018 el único caso es cilantro (arranca ago-2024): se arrastra su valor más
# temprano hacia los meses previos.
defl_ccif <- deflactores |>
  filter(grupo == "ccif", date >= fecha_inicio_serie) |>
  select(serie, date, deflactor)

fechas_ccif <- sort(unique(defl_ccif$date))

alim_serie <- base_alim |>
  left_join(ccif_map, by = c("ambito", "clave")) |>
  crossing(date = fechas_ccif) |>
  left_join(defl_ccif, by = c("serie", "date")) |>
  arrange(ambito, clave, date) |>
  group_by(ambito, clave) |>
  mutate(deflactor = na.locf(na.locf(deflactor, na.rm = FALSE), fromLast = TRUE,
                             na.rm = FALSE)) |>
  ungroup() |>
  group_by(ambito, date) |>
  summarise(canasta_alimentos = 1.20 * sum(valor_base * deflactor), .groups = "drop")

# 6.2 Vivienda y NANV: valor base nacional por ámbito * su deflactor.
base_vn <- fread(here::here("salario_digno.csv")) |>
  filter(nom_ent == "Nacional") |>
  transmute(ambito, vivienda_base = vivienda_mensual, nanv_base = NANV)

defl_viv  <- deflactores |> filter(grupo == "vivienda") |> select(date, defl_viv = deflactor)
defl_inpc <- deflactores |> filter(grupo == "inpc")     |> select(date, defl_inpc = deflactor)

# ── 7. ensamble del salario digno y salidas ───────────────────────────────────

serie <- alim_serie |>
  left_join(base_vn, by = "ambito") |>
  left_join(defl_viv,  by = "date") |>
  left_join(defl_inpc, by = "date") |>
  mutate(
    vivienda_mensual       = vivienda_base * defl_viv,
    NANV                   = nanv_base     * defl_inpc,
    alimentos_familiar     = canasta_alimentos * 4,
    nanv_familiar          = NANV * 4,
    ingreso_digno_familiar = (alimentos_familiar + vivienda_mensual + nanv_familiar) * 1.05,
    salario_digno          = ingreso_digno_familiar / 2
  ) |>
  select(date, ambito, canasta_alimentos, vivienda_mensual, NANV,
         alimentos_familiar, nanv_familiar, ingreso_digno_familiar, salario_digno) |>
  arrange(ambito, date)

fwrite(serie, file.path(dir_serie, "serie_salario_digno_nacional.csv"), bom = TRUE)

componentes <- serie |>
  select(date, ambito, Alimentos = canasta_alimentos,
         Vivienda = vivienda_mensual, NANV) |>
  pivot_longer(c(Alimentos, Vivienda, NANV),
               names_to = "componente", values_to = "valor")

fwrite(componentes, file.path(dir_serie, "serie_componentes_nacional.csv"), bom = TRUE)

# chequeo de anclaje: la serie en ago-2024 debe reproducir salario_digno.csv
chk <- serie |>
  filter(date == mes_base) |>
  select(ambito, salario_digno_serie = salario_digno) |>
  left_join(
    fread(here::here("salario_digno.csv"))[nom_ent == "Nacional",
                                           .(ambito, salario_digno_puntual = salario_digno)],
    by = "ambito"
  ) |>
  mutate(dif = salario_digno_serie - salario_digno_puntual)
message("Chequeo de anclaje (ago-2024, debe ser ~0):")
print(chk)

# ── 8. gráfica DT 2026 (salario digno nacional, Rural vs Urbano) ──────────────
# Marca dos puntos por serie: el último mes (etiqueta arriba) y el valor base
# ago-2024 (ancla ENIGH 2024; etiqueta abajo + línea vertical punteada).

ult  <- max(serie$date)
etq  <- serie |> group_by(ambito) |> filter(date == max(date)) |> ungroup()
base <- serie |> filter(date == mes_base)

g <- ggplot(serie, aes(date, salario_digno, color = ambito)) +
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

archivo <- glue("salario_digno_nacional_{format(ult, '%Y')}m{format(ult, '%m')}")
guardar_grafica_conasami(g, archivo, tamano = "ancho", dir = dir_graphs)

message("5_serie_salario_digno_nacional.R: listo. Serie y gráfica en finaldata/serie/ y graphs/serie/.")
