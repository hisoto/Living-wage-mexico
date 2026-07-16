#_______________________________________________________________________________
# utils_inpc_scraper.R
#
# Coordinación de Análisis de la Economía Laboral (CAEL) — Conasami
# Salario Digno — utilería de scraping del portal de índices de precios del INEGI
#
# Propósito: función get_inpc_serie() para descargar series del INPC (niveles/
#   índices) del portal ASP.NET del INEGI. Se extrajo de
#   5_serie_salario_digno_nacional.R (que a su vez la tomó de
#   Informes/comportamiento_precios/rscripts/datos_02.R) para compartirla entre el
#   script nacional (5) y el de entidades (6). El paquete inegiR está caído, por eso
#   se hace scraping directo.
#
# Requiere (cargar antes de sourcear): rvest, httr2, xml2, tidyverse (purrr/tibble).
#
# Uso:
#   df <- get_inpc_serie(idEstructura = "112001700010",
#                        series       = "e|865541",   # una serie del portal
#                        anio_ini = 2016, anio_fin = 2026)
#   # -> tibble(periodo, valor)  (una fila por periodo, en texto)
#
# Notas:
#   - Una llamada = GET de la página de estructura (para los tokens ASP.NET) + POST
#     de exportación. Cada idEstructura agrupa un conjunto de series (p. ej. INPC
#     general, CCIF, objeto del gasto nacional, o una entidad federativa).
#   - Devuelve exactamente periodo + valor para la serie pedida (mismo contrato que
#     usaba el script 5 antes de la extracción).
#_______________________________________________________________________________

# ── scraping de una serie del portal de índices de precios del INEGI ───────────

get_inpc_serie <- function(idEstructura, series,
                           anio_ini = 2016,
                           anio_fin = as.integer(format(Sys.Date(), "%Y"))) {

  url_estructura <- paste0(
    "https://www.inegi.org.mx/app/indicesdepreciosv2/Estructura.aspx?idEstructura=",
    idEstructura
  )

  html_get <- request(url_estructura) |>
    req_perform() |>
    resp_body_string() |>
    read_html()

  viewstate     <- html_get |> html_element("#__VIEWSTATE")          |> html_attr("value")
  viewstate_gen <- html_get |> html_element("#__VIEWSTATEGENERATOR") |> html_attr("value")
  event_val     <- html_get |> html_element("#__EVENTVALIDATION")    |> html_attr("value")

  body <- list(
    "__VIEWSTATE"          = viewstate,
    "__VIEWSTATEGENERATOR" = viewstate_gen,
    INPTipoExporta         = "HTML",
    idEstructura           = idEstructura,
    `_formato`             = "HTML",
    `_anioI`               = anio_ini,
    `_anioF`               = anio_fin,
    `_meta`                = 0,
    `_tipo`                = "Niveles",
    `_info`                = "Índices",
    `_orient`              = "vertical",
    esquema                = 0,
    pf                     = "inp",
    cuadro                 = idEstructura,
    `_series`              = series,
    cveEstructura          = idEstructura
  )
  if (!is.na(event_val)) body$`__EVENTVALIDATION` <- event_val

  html_post <- request(
    "https://www.inegi.org.mx/app/indicesdepreciosv2/Exportacion.aspx"
  ) |>
    req_method("POST") |>
    req_body_form(!!!body) |>
    req_perform() |>
    resp_body_string() |>
    read_html()

  filas <- html_post |>
    html_elements("table#TableCuadro tr") |>
    keep(~ length(html_elements(.x, "td.fecha")) == 1)

  if (length(filas) == 0) stop("Sin filas de datos para series = ", series)

  map_dfr(filas, function(tr) {
    tds <- tr |> html_elements("td")
    tibble(periodo = html_text(tds[[1]], trim = TRUE),
           valor   = html_text(tds[[2]], trim = TRUE))
  })
}

# ── scraping de VARIAS series de una misma idEstructura en una sola consulta ────
# El portal acepta varias series separadas por COMA en `_series`
# ("e|id1,e|id2,..."), y devuelve una columna de valor por serie. La fila "Fecha"
# del HTML trae el ID de cada columna, así que las columnas se alinean a su serie
# POR ID PARSEADO (no por el orden pedido), lo que hace la descarga por lotes
# robusta. Devuelve formato largo (serie, periodo, valor en texto). Usada por
# 6_serie_salario_digno_entidades.R para bajar las ~1,700 series estatales en
# pocas consultas (todas viven en la estructura "por entidad federativa").

get_inpc_series_lote <- function(idEstructura, series_ids,
                                 anio_ini = 2016,
                                 anio_fin = as.integer(format(Sys.Date(), "%Y"))) {

  series_str <- paste0("e|", series_ids, collapse = ",")

  url_estructura <- paste0(
    "https://www.inegi.org.mx/app/indicesdepreciosv2/Estructura.aspx?idEstructura=",
    idEstructura
  )

  html_get <- request(url_estructura) |>
    req_perform() |>
    resp_body_string() |>
    read_html()

  viewstate     <- html_get |> html_element("#__VIEWSTATE")          |> html_attr("value")
  viewstate_gen <- html_get |> html_element("#__VIEWSTATEGENERATOR") |> html_attr("value")
  event_val     <- html_get |> html_element("#__EVENTVALIDATION")    |> html_attr("value")

  body <- list(
    "__VIEWSTATE"          = viewstate,
    "__VIEWSTATEGENERATOR" = viewstate_gen,
    INPTipoExporta         = "HTML",
    idEstructura           = idEstructura,
    `_formato`             = "HTML",
    `_anioI`               = anio_ini,
    `_anioF`               = anio_fin,
    `_meta`                = 0,
    `_tipo`                = "Niveles",
    `_info`                = "Índices",
    `_orient`              = "vertical",
    esquema                = 0,
    pf                     = "inp",
    cuadro                 = idEstructura,
    `_series`              = series_str,
    cveEstructura          = idEstructura
  )
  if (!is.na(event_val)) body$`__EVENTVALIDATION` <- event_val

  html_post <- request(
    "https://www.inegi.org.mx/app/indicesdepreciosv2/Exportacion.aspx"
  ) |>
    req_method("POST") |>
    req_body_form(!!!body) |>
    req_perform() |>
    resp_body_string() |>
    read_html()

  filas <- html_post |> html_elements("table#TableCuadro tr")
  celdas <- function(tr) html_text(html_elements(tr, "td, th"), trim = TRUE)

  # fila "Fecha": IDs de serie por columna (la 1a celda es la etiqueta "Fecha")
  etiquetas <- map_chr(filas, ~ { c <- celdas(.x); if (length(c) > 0) c[[1]] else NA_character_ })
  idx_fecha <- which(etiquetas == "Fecha")[1]
  if (is.na(idx_fecha)) stop("No se encontró la fila 'Fecha' (IDs) para idEstructura ", idEstructura)
  ids_col <- celdas(filas[[idx_fecha]])[-1]

  # filas de datos: exactamente una celda de fecha (td.fecha)
  datos <- filas |> keep(~ length(html_elements(.x, "td.fecha")) == 1)
  if (length(datos) == 0) stop("Sin filas de datos para el lote (idEstructura ", idEstructura, ")")

  map_dfr(datos, function(tr) {
    cel <- celdas(tr)
    valores <- cel[-1]
    if (length(valores) != length(ids_col))
      stop("Desalineación de columnas: ", length(valores), " valores vs ",
           length(ids_col), " IDs")
    tibble(serie = ids_col, periodo = cel[[1]], valor = valores)
  })
}
