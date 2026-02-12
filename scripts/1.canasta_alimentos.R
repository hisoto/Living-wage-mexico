#_______________________________________________________________________________

# Título: Canasta alimentaria 

# Objetivo: 

  # Este script es para la construcción del componente alimentario 
  # de la Canasta Digna

# Autor: 

  # Coordinación de Análisis de la Economía Laboral
  # de la Comisión Nacional de los Salarios Mínimos 

# inputs: ENIGH 2024

  # Se pueden descargar las bases de la Encuesta Nacional de Ingresos y Gastos 
  # de los Hogares (ENIGH) 2022 en formato DTA en: 
  
  # https://www.inegi.org.mx/programas/enigh/nc/2022/#microdatos

  # Base de Vivienda  
  # Base de Población
  # Base de Hogares 
  # Base Concentrado
  # Base Gastos de hogar 

# Nota:

  # Se recomienda crear un proyecto para el manejo adecuado del directorio. 
  # Este debe incluir las siguientes carpetas: 
  # finaldata (aquí se guardan las bases finales)
  # data (aquí se guardan las bases de la ENIGH)
  # graphs (aquí se guardan las gráficas generadas)
  # scripts (para resguardar el script y el tema grafico de la Comisión)

#_______________________________________________________________________________

rm(list = ls()); gc()

if (!require(pacman))
  install.packages("pacman")
library(pacman)

p_load(
  tidyverse,
  stringr,
  Hmisc,
  EnvStats,
  survey,
  srvyr,
  data.table,
  bit64,
  statar,
  foreign,
  openxlsx,
  extrafont,
  readxl,
  patchwork,
  haven
)

source("scripts/theme_conasami.R")

# Indicadores de vivienda ------------------------------------------------------

vivienda <- read_dta("data/enigh2024_ns_viviendas_dta/viviendas.dta")

vivienda <- vivienda %>%
  mutate(
    across(
      where(is.character) &
        !matches("folioviv"),
      ~ suppressWarnings(as.numeric(.))
    ),
    rural = ifelse(substr(folioviv, 3, 3) == "6", 1, 0),
    habitan_p_cuarto = tot_resid / cuart_dorm,
    agua_e = ifelse(agua_ent == 1 | agua_ent == 2, 1, 0),
    agua_ab = ifelse(ab_agua >= 1 &
                       ab_agua <= 4 |
                       ((ab_agua == 6 &
                           procaptar == 1) |
                          (agua_noe == 5) | (agua_noe == 6 & procaptar == 1)
                       ), 1, 0),
    agua_d = ifelse(dotac_agua == 1 | dotac_agua == 2, 1, 0),
    paredes = ifelse((mat_pared == 8) |
                       (mat_pared == 7 & rural == 1), 1, 0),
    techos = ifelse(mat_techos == 10 | mat_techos == 9, 1, 0),
    pisos = ifelse((mat_pisos == 2 | mat_pisos == 3), 1, 0),
    probms = ifelse((
      p_pandeos == 1 |
        p_levanta == 1 | p_humedad == 1 | p_fractura == 1
    ), 0, 1),
    wc = ifelse(excusado == 1 | excusado == 2, 1, 0),
    twc = ifelse(uso_compar == 2, 1, 0),
    awc = ifelse(sanit_agua == 1, 1, 0),
    red = ifelse(drenaje == 1, 1, 0)
  ) %>%
  mutate(
    # acceso a agua entubada dentro de la vivienda o del terreno
    i.agua = ifelse(agua_e == 1 &
                      agua_ab == 1 &
                      agua_d == 1, 1, 0),
    # materiales adecuados
    i.materiales = ifelse(paredes == 1 &
                            techos == 1 &
                            pisos == 1 &
                            probms == 1, 1, 0),
    # sin hacinamiento
    i.hacinamiento = ifelse(habitan_p_cuarto <= 2.5, 1, 0),
    # retrete sin uso compartido y con drenaje a red pública
    i.saneamiento = ifelse(wc == 1 &
                             twc == 1 &
                             awc == 1 &
                             red == 1, 1, 0)
  )

indicadores_vivienda <- vivienda %>%
  # Seleccionamos únicamente las dicotómicas creadas
  select(folioviv, rural, starts_with("i."))

rm(vivienda)

# Indicadores poblacionales ----------------------------------------------------

poblacion <- read_dta("data/enigh2024_ns_poblacion_dta/poblacion.dta")

poblacion <- poblacion %>%
  mutate(
    # identificamos niños sin educación inicial por falta de recursos o sin acceso
    id.sin_edu_ini = ifelse(edad < 3 &
                              edu_ini == 6 &
                              no_asis %in% c(3, 5), 1, 0),
    # identificamos infancias que no asistan a la escuela
    id.sis_edu = ifelse(edad >= 3 &
                          edad <= 18 & asis_esc == 2, 1, 0),
    # identificamos infancias con alfabetismo (meditar)
    id.alfabetismo = ifelse(edad >= 7 &
                              edad <= 18 & alfabetism == 2, 1, 0)
  )

# identificamos a los hogares con alguna de estas privaciones

indicadores_edu <- poblacion %>%
  group_by(folioviv, foliohog) %>%
  summarise(i.edu = ifelse(any(id.sin_edu_ini == 1 |
                                 id.sis_edu == 1), 0, 1)) %>%
  ungroup()

rm(poblacion)

# Indicadores del hogar --------------------------------------------------------

hogares <- read_dta("data/enigh2024_ns_hogares_dta/hogares.dta")

# queremos identificar hogares con cualquier forma de privación alimentaria

indicadores_alim <- hogares %>%
  select(-c(acc_alim18)) %>%
  mutate(i.alim = if_else(if_any(starts_with("acc_alim"), ~ .x == 1), 0, 1)) %>%
  select(folioviv, foliohog, i.alim)

rm(hogares)

# Indicadores de Bienestar -----------------------------------------------------

indicadores <- full_join(indicadores_vivienda, indicadores_edu, by = c("folioviv")) %>%
  full_join(indicadores_alim, by = c("folioviv", "foliohog"))


rm(indicadores_vivienda, indicadores_edu, indicadores_alim)

concentrado <- read_dta("data/enigh2024_ns_concentradohogar_dta/concentradohogar.dta")

concentrado <- concentrado %>%
  left_join(indicadores, by = c("folioviv", "foliohog")) %>%
  mutate(
    i.exceso.g.alim = ifelse(alimentos / gasto_mon >= .75, 0, 1),
    rural = ifelse(substr(folioviv, 3, 3) == 6, 1, 0)
  ) %>%
  rename(factor_exp = 8)

rm(indicadores)

tabla.indicadores <- concentrado %>%
  #group_by() %>%
  summarise(
    hogares = sum(factor_exp, na.rm = TRUE),
    i.agua = sum(factor_exp[i.agua == 0], na.rm = TRUE),
    i.materiales = sum(factor_exp[i.materiales == 0], na.rm = TRUE),
    i.hacinamiento = sum(factor_exp[i.hacinamiento == 0], na.rm = TRUE),
    i.saneamiento = sum(factor_exp[i.saneamiento == 0], na.rm = TRUE),
    i.edu = sum(factor_exp[i.edu == 0], na.rm = TRUE),
    i.alim = sum(factor_exp[i.alim == 0], na.rm = TRUE),
    i.exceso.g.alim = sum(factor_exp[i.exceso.g.alim == 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0))) %>%
  pivot_longer(cols = -hogares,
               names_to = "indicador",
               values_to = "cantidad")

fwrite(tabla.indicadores, "finaldata/alimentos/1.indicadores_bienestar.csv")

rm(tabla.indicadores)

# percentiles del ingreso corriente percapita ----------------------------------

concentrado <- concentrado %>%
  mutate(ing_p = (ing_cor) / tot_integ) %>%
  arrange(ing_p) %>%
  # Creamos el percentil nacional, se usa para crear las gráficas e identificar los indicadores que nos interesan
  mutate(percentil_nacional = xtile(ing_p, n = 100, wt = factor_exp)) %>%
  # Agrupamos por ámbito
  group_by(rural) %>%
  # Se crea el percentil por ámbito, como seleccionamos un estrato por ámbito las estimaciones igual se hacen por ambito
  mutate(percentil = xtile(ing_p, n = 100, wt = factor_exp)) %>%
  ungroup()

tabla.percentil.indicadores <- concentrado %>%
  group_by(percentil_nacional) %>%
  # Se obtiene el promedio de hogares que no sufren de la carencia por pencentil nacional
  summarise(
    i.agua.p = sum(factor_exp[i.agua == 1], na.rm = TRUE) / sum(factor_exp, na.rm = TRUE),
    i.materiales.p = sum(factor_exp[i.materiales == 1], na.rm = TRUE) / sum(factor_exp, na.rm = TRUE),
    i.hacinamiento.p = sum(factor_exp[i.hacinamiento == 1], na.rm = TRUE) / sum(factor_exp, na.rm = TRUE),
    i.saneamiento.p = sum(factor_exp[i.saneamiento == 1], na.rm = TRUE) / sum(factor_exp, na.rm = TRUE),
    i.edu.p = sum(factor_exp[i.edu == 1], na.rm = TRUE) / sum(factor_exp, na.rm = TRUE),
    i.alim.p = sum(factor_exp[i.alim == 1], na.rm = TRUE) / sum(factor_exp, na.rm = TRUE),
    i.exceso.g.alim.p = sum(factor_exp[i.exceso.g.alim == 1], na.rm = TRUE) / sum(factor_exp, na.rm = TRUE)
  )

fwrite(tabla.percentil.indicadores,
       "finaldata/alimentos/2.percentiles_indicadores_bienestar.csv")

# Gráficas de Indicadores de Bienestar -----------------------------------------

g.alimento <- ggplot(tabla.percentil.indicadores) +
  geom_line(mapping = aes(x = percentil_nacional, y = i.alim.p * 100, color = "Sin falta de alimentos")) +
  geom_line(
    mapping = aes(x = percentil_nacional, y = i.exceso.g.alim.p * 100, color = "Exceso de gasto alimento")
  ) +
  geom_point(mapping = aes(x = percentil_nacional, y = i.alim.p * 100, color = "Sin falta de alimentos")) +
  geom_point(
    mapping = aes(x = percentil_nacional, y = i.exceso.g.alim.p * 100, color = "Exceso de gasto alimento")
  ) +
  scale_color_manual(values = c(
    "Sin falta de alimentos" = "#611232",
    "Exceso de gasto alimento" = "#98989A"
  )) +
  labs(
    title = "Indicadores alimentarios",
    x = "Percentil",
    y = "(%) de hogares",
    color = "",
    caption = ""
  ) +
  theme_conasami()  +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 18, color = "white"),
    axis.title = element_text(size = 20, color = "white"),
    legend.text = element_text(size = 16, color = "white"),
    legend.title = element_text(color = "white"),
    plot.caption = element_text(color = "white"),
    axis.text = element_text(color = "white")
  )

ggsave(
  "graphs/indicadores_alimentarios.png",
  g.alimento,
  width = 9,
  height = 6,
  dpi = 300
)

g.vivienda <- ggplot(tabla.percentil.indicadores) +
  geom_line(mapping = aes(x = percentil_nacional, y = i.materiales.p * 100, color = "Habitabilidad")) +
  geom_line(mapping = aes(x = percentil_nacional, y = i.hacinamiento.p *
                            100, color = "Sin hacinamiento")) +
  geom_point(mapping = aes(x = percentil_nacional, y = i.materiales.p * 100, color = "Habitabilidad")) +
  geom_point(mapping = aes(x = percentil_nacional, y = i.hacinamiento.p *
                             100, color = "Sin hacinamiento")) +
  scale_color_manual(values = c(
    "Habitabilidad" = "#611232",
    "Sin hacinamiento" = "#98989A"
  )) +
  labs(
    title = "Indicadores vivienda",
    x = "Percentil",
    y = "(%) de hogares",
    caption = "",
    color = ""
  ) +
  theme_conasami()  +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 16)
  )

g.vivienda

ggsave(
  "graphs/indicadores_vivienda.png",
  g.vivienda,
  width = 9,
  height = 6,
  dpi = 300
)

g.servicios <- ggplot(tabla.percentil.indicadores) +
  geom_line(mapping = aes(x = percentil_nacional, y = i.agua.p * 100, color = "Acceso a fuentes de agua")) +
  geom_line(mapping = aes(x = percentil_nacional, y = i.saneamiento.p *
                            100, color = "Servicios de baño")) +
  geom_point(mapping = aes(x = percentil_nacional, y = i.agua.p * 100, color = "Acceso a fuentes de agua")) +
  geom_point(mapping = aes(x = percentil_nacional, y = i.saneamiento.p *
                             100, color = "Servicios de baño")) +
  scale_color_manual(values = c(
    "Acceso a fuentes de agua" = "#98989A",
    "Servicios de baño" = "#611232"
  )) +
  labs(
    title = "Indicadores servicios",
    x = "Percentil",
    y = "(%) de hogares",
    caption = "",
    color = ""
  ) +
  theme_conasami()  +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 16)
  )

g.edu <- ggplot(tabla.percentil.indicadores) +
  geom_line(mapping = aes(x = percentil_nacional, y = i.edu.p * 100)) +
  geom_point(mapping = aes(x = percentil_nacional, y = i.edu.p * 100)) +
  labs(
    title = "Indicador de educación",
    x = "Percentil",
    y = "(%) de hogares",
    caption = ""
  ) +
  theme_conasami()  +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 16)
  )

g.indicadores <- g.alimento + g.vivienda + g.servicios + g.edu +
  plot_layout(ncol = 2, nrow = 2) +
  plot_annotation(title = "",
                  caption = "",
                  theme = theme_conasami())

g.indicadores

ggsave(
  "graphs/indicadores_bienestar.png",
  g.indicadores,
  width = 18,
  height = 10,
  dpi = 300
)

rm(tabla.percentil.indicadores, list = ls(pattern = "g\\."))

# Indice de bienestar ----------------------------------------------------------

concentrado <- concentrado %>%
  mutate(indice = (i.saneamiento + i.materiales + i.edu + i.alim))

# Identificamos el porcentaje de hogares

design <- concentrado %>% # Se define el diseño muestral de la encuesta
  as_survey_design(weights = factor_exp,
                   strata = est_dis,
                   ids = upm)

resultados_svyr <- svyby(
  # svyby genera estadísticas de subconjuntos de la encuesta, en este caso cada uno se define por la v. quintil y ambito
  ~ I(indice >= 2) + I(indice >= 1) + I(indice == 4),
  # Se identifican los hogares que cumplen con los criterios
  by = ~ cut(
    percentil,
    breaks = quantile(concentrado$percentil, probs = seq(0, 1, 0.1)),
    labels = 1:10,
    include.lowest = TRUE
  ) + rural,
  design = design,
  FUN = svymean,
  na.rm = TRUE
)

# Para darle formato
resultados_svyr <- resultados_svyr %>%
  rename(
    percentil = 1,
    criterio_intermedio = 4,
    criterio_union = 6,
    criterio_interseccion = 8
  ) %>%
  select(percentil,
         rural,
         criterio_intermedio,
         criterio_union,
         criterio_interseccion) %>%
  mutate(
    criterio_intermedio = round(criterio_intermedio * 100, 2),
    criterio_union = round(criterio_union * 100, 2),
    criterio_interseccion = round(criterio_interseccion * 100, 2)
  )

fwrite(resultados_svyr, "finaldata/alimentos/3.tabla_indice_bienestar.csv")

rm(resultados_svyr, design)

# Estrato de referencia --------------------------------------------------------

estrato <- concentrado %>%
  filter((rural == 0 & percentil >= 41 & percentil <= 61) |
           (rural == 1 & percentil >= 80 & percentil <= 100)) %>%
  select(folioviv,
         foliohog,
         rural,
         alimentos,
         gasto_mon,
         ing_cor,
         est_dis,
         upm,
         factor_exp) %>%
  mutate(estrato_ref = 1)

rm(concentrado)

# obtenemos el gasto medio


design <- estrato %>%
  as_survey_design(weights = factor_exp,
                   strata = est_dis,
                   ids = upm)

options(survey.lonely.psu = "adjust")

tabla.medias <- design %>%
  group_by(rural) %>%
  summarise(
    n = n(),
    alimentos = survey_mean(
      alimentos / 3,
      vartype = c("se" , "cv", "ci"),
      level = 0.95,
      na.rm = TRUE
    ),
    gasto_mon = survey_mean(
      gasto_mon / 3,
      vartype = c("se" , "cv", "ci"),
      level = 0.95,
      na.rm = TRUE
    ),
    ing_cor = survey_mean(
      ing_cor / 3,
      vartype = c("se" , "cv", "ci"),
      level = 0.95,
      na.rm = TRUE
    ),
    n_hogares = survey_total(
      1,
      vartype = c("se" , "cv", "ci"),
      level = 0.95,
      na.rm = TRUE
    )
  )

num_hog_urbano <- tabla.medias %>% 
  select(rural, n_hogares) %>% 
  filter(rural == 0) %>% 
  pull(n_hogares)

num_hog_rural <- tabla.medias %>% 
  select(rural, n_hogares) %>% 
  filter(rural == 1) %>% 
  pull(n_hogares)


fwrite(tabla.medias, "finaldata/alimentos/4.tabla_medias_estrato.csv")

rm(tabla.medias, design)

estrato <- estrato %>%
  select(folioviv, foliohog, estrato_ref)

# Productos representativos ----------------------------------------------------

claves <- c(
  "111111",
  "111112",
  "111113",
  "111114",
  "111115",
  "111116",
  "181171",
  "181113",
  "181156",
  "181114",
  "181153",
  "181115",
  "181116",
  "181117",
  "181158",
  "181118",
  "181119",
  "181122",
  "181121",
  "181120",
  "181124",
  "181125",
  "181126",
  "181123",
  "181127",
  "181157",
  "181128",
  "181129",
  "181130",
  "181132",
  "181131",
  "181160",
  "181133",
  "181159",
  "181163",
  "181141",
  "181165",
  "181142",
  "181164",
  "181161",
  "181166",
  "181140",
  "181167",
  "181136",
  "181137",
  "181138",
  "181135",
  "181139",
  "181134",
  "181155",
  "181144",
  "181145",
  "181143",
  "181168",
  "111125",
  "111122",
  "111126",
  "111121",
  "111123",
  "111124",
  "181162",
  "181170"
)

gasto_hogar <- read_dta("data/enigh2024_ns_gastoshogar_dta/gastoshogar.dta") %>%
  left_join(estrato, by = c("folioviv", "foliohog")) %>%
  rename(factor_exp = factor) %>%
  mutate(rural = ifelse(substr(folioviv, 3, 3) == "6", 1, 0),
         clase = substr(clave, 1, 2)) %>%
  filter(estrato_ref == 1, clase == "01" |
           clave %in% claves)

rm(estrato)

gasto_hogar_ind <- gasto_hogar %>%
  distinct(folioviv, foliohog, clave, .keep_all = TRUE) %>%
  mutate(consumio = 1)

design <- gasto_hogar_ind %>%
  as_survey_design(weights = factor_exp,
                   strata = est_dis,
                   ids = upm)

productos_survey <- design %>% # Este código nos da el total de hogares que compraron un determinado producto
  group_by(rural, clave) %>%
  summarise(pct_hogares = survey_total(
    consumio,
    vartype = c("se" , "cv", "ci"),
    level = 0.95
  ),
  .groups = "drop")

productos <- productos_survey %>%
  mutate(porcentaje = ifelse(
    rural == 1,
    pct_hogares / num_hog_rural,
    pct_hogares / num_hog_urbano
  )) %>%
  filter(porcentaje >= 0.1 & rural == 0 |
           porcentaje >= 0.08 & rural == 1)

ca_urbano <- productos %>%
  filter(rural == 0) %>%
  select(clave) %>%
  pull()

ca_rural <- productos %>%
  filter(rural == 1) %>%
  select(clave) %>%
  pull()


rm(productos_survey, design, gasto_hogar_ind, productos)

# Cantidades medias consumidas por el estrato ----------------------------------

design <- gasto_hogar %>%
  as_survey_design(weights = factor_exp,
                   strata = est_dis,
                   ids = upm)

cantidades_urbano <- design %>%
  filter(rural == 0, clave %in% ca_urbano) %>%
  group_by(clave) %>%
  summarise(cantidad = survey_median(
    cantidad,
    na.rm = TRUE,
    vartype = c("se" , "cv", "ci"),
    level = 0.95
  ))

cantidades_rural <- design %>%
  filter(rural == 1, clave %in% ca_rural) %>%
  group_by(clave) %>%
  summarise(cantidad = survey_median(
    cantidad,
    na.rm = TRUE,
    vartype = c("se" , "cv", "ci"),
    level = 0.95
  ))

fwrite(cantidades_rural, "finaldata/alimentos/5.cantidades_ca_rural.csv")
fwrite(cantidades_urbano, "finaldata/alimentos/5.cantidades_ca_urbano.csv")

rm(design, gasto_hogar)

# Estimación del precio --------------------------------------------------------

gasto_hogar <- read_dta("data/enigh2024_ns_gastoshogar_dta/gastoshogar.dta") %>%
  mutate(
    rural = ifelse(substr(folioviv, 3, 3) == "6", "rural", "urbano"),
    cve_ent = as.numeric(entidad),
    nom_ent = case_when(
      cve_ent == 1 ~ "Aguascalientes",
      cve_ent == 2 ~ "Baja California",
      cve_ent == 3 ~ "Baja California Sur",
      cve_ent == 4 ~ "Campeche",
      cve_ent == 5 ~ "Coahuila",
      cve_ent == 6 ~ "Colima",
      cve_ent == 7 ~ "Chiapas",
      cve_ent == 8 ~ "Chihuahua",
      cve_ent == 9 ~ "Ciudad de México",
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
    costo_medio = gasto / cantidad
  ) %>%
  group_by(clave, nom_ent, rural) %>%
  mutate(
    IQR = IQR(costo_medio, na.rm = TRUE),
    Q1 = quantile(costo_medio, 0.25, na.rm = TRUE),
    Q3 = quantile(costo_medio, 0.75, na.rm = TRUE),
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR
  ) %>%
  filter(costo_medio >= Lower_Bound &
           costo_medio <= Upper_Bound) %>%
  ungroup() %>%
  select(-IQR, -Q1, -Q3, -Lower_Bound, -Upper_Bound)


ambito <- c("rural", "urbano")

resultados <- list()


for (i in ambito) {
  gc()
  
  gasto_i <- gasto_hogar %>%
    filter(clave %in% get(paste0("ca_", i), inherits = TRUE), rural == i)
  
  design <- gasto_i %>%
    as_survey_design(weights = factor,
                     ids = upm,
                     strata = est_dis)
  
  gc()
  
  precios_i <- design %>%
    group_by(clave) %>%
    summarise(precio = survey_median(
      costo_medio,
      na.rm = TRUE,
      vartype = c("se" , "cv", "ci"),
      level = 0.95
    )) %>%
    select(clave, precio) %>%
    mutate(nom_ent = "Nacional")
  
  precios_i_ent <- design %>%
    group_by(clave, nom_ent) %>%
    summarise(precio = survey_median(
      costo_medio,
      na.rm = TRUE,
      vartype = c("se" , "cv", "ci"),
      level = 0.95
    )) %>%
    select(clave, nom_ent, precio)
  
  precios_i <- precios_i %>%
    rbind(precios_i_ent) %>%
    left_join(get(paste0("cantidades_", tolower(i)), inherits = TRUE), by = "clave") %>%
    mutate(valor = precio * cantidad)
  
  fwrite(precios_i, paste0("finaldata/alimentos/6.precios_ca_", i, ".csv"))
  
  resultados[[i]] <- precios_i
}

rm(design, precios_i_ent, gasto_i, precios_i)

canasta_urbana <- resultados[["urbano"]] %>%
  group_by(nom_ent) %>%
  summarise(valor = sum(valor)) %>%
  mutate(canasta_alimentos = valor * 1.20, ambito = "Urbano")


canasta_rural <- resultados[["rural"]] %>%
  group_by(nom_ent) %>%
  summarise(valor = sum(valor)) %>%
  mutate(canasta_alimentos = valor * 1.20, ambito = "Rural")

canasta <- bind_rows(canasta_urbana, canasta_rural) %>%
  arrange(ambito, canasta_alimentos)


fwrite(canasta, "finaldata/alimentos/7.canasta_alimentaria.csv")
