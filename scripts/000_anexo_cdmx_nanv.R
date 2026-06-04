#_______________________________________________________________________________

# Título: ANEXO — Comparación de ámbito rural y urbano (Ciudad de México)

# Objetivo:

  # Sustentar la subsección "Comparación de ámbito rural y urbano" del componente
  # NANV. Para la Ciudad de México (única entidad donde la Canasta Digna rural
  # supera a la urbana) se documenta:
  #   1) la relación SCAM monotónica entre el alquiler per cápita y la
  #      probabilidad acumulada del gasto corriente por ámbito, distinguiendo a
  #      los hogares con vivienda digna de los que no la tienen, y
  #   2) un radar de los cuatro criterios de vivienda digna de ONU-Hábitat
  #      (habitabilidad, asequibilidad, tenencia, disponibilidad) por ámbito,
  #      que evidencia la menor disponibilidad de servicios en el ámbito rural.

# Autor:

  # Coordinación de Análisis de la Economía Laboral
  # de la Comisión Nacional de los Salarios Mínimos

# inputs:
  #   data/enigh2024_ns_concentradohogar_dta/concentradohogar.dta
  #   data/viv_dig.dta  (output de scripts/2. Vivienda.do)
# outputs:
  #   graphs/relacion/cdmx_dignas_gasto.png  (figura @fig-cdmx-scam)
  #   graphs/relacion/cdmx_radar.png         (figura @fig-cdmx-radar)

# Nota:
  # Independiente del pipeline principal (1 → 4); requiere viv_dig.dta del
  # componente de vivienda. Diagnóstico/anexo del componente vivienda-NANV.

#_______________________________________________________________________________

rm(list = ls()); gc()
options(scipen = 999)

if (!require(pacman))
  install.packages("pacman")

pacman::p_load(
  tidyverse,
  survey,
  srvyr,
  data.table,
  scam,
  statar,
  Hmisc,
  haven
)

source("scripts/theme_conasami.R")

dir.create("graphs/relacion", showWarnings = FALSE, recursive = TRUE)

# ── Carga y preparación ───────────────────────────────────────────────────────

vivienda <- read_dta("data/viv_dig.dta")

concentrado <- read_dta(
  "data/enigh2024_ns_concentradohogar_dta/concentradohogar.dta"
)

concentrado <- concentrado |>
  left_join(vivienda, by = c("folioviv", "foliohog")) |>
  replace_na(
    list(
      ten_f          = 0,
      disponibilidad = 0,
      aseq           = 0,
      habitabilidad  = 0,
      viv_dig        = 0
    )
  )

rm(vivienda)

concentrado <- concentrado |>
  mutate(
    rural   = ifelse(substr(folioviv, 3, 3) == "6", 1, 0),
    cve_ent = as.numeric(substr(folioviv, 1, 2)),
    nom_ent = case_when(
      cve_ent ==  1 ~ "Aguascalientes",
      cve_ent ==  2 ~ "Baja California",
      cve_ent ==  3 ~ "Baja California Sur",
      cve_ent ==  4 ~ "Campeche",
      cve_ent ==  5 ~ "Coahuila",
      cve_ent ==  6 ~ "Colima",
      cve_ent ==  7 ~ "Chiapas",
      cve_ent ==  8 ~ "Chihuahua",
      cve_ent ==  9 ~ "Ciudad de México",
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
    viv         = vivienda + estim_alqu,
    viv_p       = viv / tot_integ,
    gasto_mon_p = gasto_mon / tot_integ
  ) |>
  filter(viv != 0)

# Recorte de outliers por entidad y ámbito (p5–p95 del gasto corriente)
concentrado <- concentrado |>
  arrange(gasto_mon) |>
  group_by(nom_ent, rural) |>
  mutate(percentil = xtile(gasto_mon, n = 100, wt = factor)) |>
  filter(percentil >= 5 & percentil <= 95) |>
  ungroup() |>
  group_by(nom_ent) |>
  mutate(gasto_p_cum = wtd.rank(gasto_mon_p, factor) / sum(factor)) |>
  filter(!is.na(gasto_p_cum), !is.na(viv_p)) |>
  ungroup()

# ── Ciudad de México: SCAM por ámbito con hogares dignos / no dignos ───────────

cdmx <- concentrado |>
  filter(nom_ent == "Ciudad de México") |>
  arrange(gasto_p_cum)

modelo_urbano <- scam(viv_p ~ s(gasto_p_cum, bs = "mpi"),
                      data = cdmx |> filter(rural == 0))
modelo_rural  <- scam(viv_p ~ s(gasto_p_cum, bs = "mpi"),
                      data = cdmx |> filter(rural == 1))

cdmx <- cdmx |>
  mutate(
    pred  = ifelse(
      rural == 0,
      predict.scam(modelo_urbano, newdata = cdmx),
      predict.scam(modelo_rural,  newdata = cdmx)
    ),
    rural = ifelse(rural == 1, "Rural", "Urbano")
  )

ggplot(cdmx) +
  geom_point(aes(x = gasto_p_cum, y = viv_p, color = as.factor(viv_dig))) +
  facet_wrap(~ rural, scales = "free_y") +
  geom_line(aes(x = gasto_p_cum, y = pred), color = "red", linewidth = 1) +
  scale_color_manual(
    values = c("0" = "#98989A", "1" = "#611232"),
    labels = c("No digna", "Digna")
  ) +
  labs(
    x     = "Probabilidad acumulada del gasto corriente",
    y     = "Alquiler per cápita",
    color = ""
  ) +
  theme_conasami() +
  theme(
    axis.title = element_text(size = 10, color = "black"),
    axis.text  = element_text(color = "black"),
    strip.text = element_text(color = "black")
  )

ggsave("graphs/relacion/cdmx_dignas_gasto.png", width = 10, height = 6)

# ── Ciudad de México: radar de criterios de vivienda digna por ámbito ──────────

design <- cdmx |>
  as_survey_design(weights = factor, strata = est_dis, ids = upm)

indicadores <- design |>
  group_by(rural) |>
  summarise(
    habitabilidad  = survey_mean(habitabilidad == 1, vartype = "ci"),
    aseq           = survey_mean(aseq == 1, vartype = "ci"),
    ten_f          = survey_mean(ten_f == 1, vartype = "ci"),
    disponibilidad = survey_mean(disponibilidad == 1, vartype = "ci")
  ) |>
  select(-ends_with("_low"), -ends_with("_upp")) |>
  pivot_longer(cols = -rural, names_to = "indicador", values_to = "valor") |>
  mutate(
    indicador = case_when(
      indicador == "habitabilidad"  ~ "Habitabilidad",
      indicador == "aseq"           ~ "Asequibilidad",
      indicador == "ten_f"          ~ "Tenencia",
      indicador == "disponibilidad" ~ "Disponibilidad"
    ),
    indicador = factor(
      indicador,
      levels = c("Habitabilidad", "Asequibilidad", "Tenencia", "Disponibilidad")
    )
  )

etiquetas_y <- data.frame(
  indicador = levels(indicadores$indicador)[1],
  valor     = seq(0.2, 1, by = 0.2),
  etiqueta  = scales::percent(seq(0.2, 1, by = 0.2), accuracy = 1)
)

ggplot(indicadores, aes(x = indicador, y = valor,
                        group = indicador, color = indicador)) +
  geom_hline(yintercept = seq(0.2, 1, by = 0.2),
             color = "grey85", linewidth = 0.4) +
  geom_text(data = etiquetas_y,
            aes(x = indicador, y = valor, label = etiqueta),
            color = "grey40", size = 3,
            inherit.aes = FALSE, show.legend = FALSE) +
  geom_segment(data = distinct(indicadores, indicador),
               aes(x = indicador, y = 0, xend = indicador, yend = 1),
               color = "grey80", linewidth = 0.4,
               inherit.aes = FALSE, show.legend = FALSE) +
  geom_point(size = 4, alpha = 0.9) +
  coord_polar() +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(
    name   = NULL,
    values = c(
      Habitabilidad  = "#611232",
      Asequibilidad  = "#98989A",
      Tenencia       = "#A57F2C",
      Disponibilidad = "#1E5B4F"
    )
  ) +
  facet_wrap(~ rural) +
  theme_conasami() +
  theme(
    panel.grid      = element_blank(),
    axis.title      = element_blank(),
    axis.text.y     = element_blank(),
    axis.ticks      = element_blank(),
    axis.line       = element_blank(),
    strip.text      = element_text(color = "black"),
    legend.position = "bottom"
  ) +
  labs(x = "", y = "")

ggsave("graphs/relacion/cdmx_radar.png", width = 8, height = 5)

# ── Tabla auxiliar: proporción de viviendas dignas por entidad y ámbito ────────

tabla_dignas <- concentrado |>
  group_by(nom_ent, rural) |>
  summarise(
    prop_digna          = weighted.mean(viv_dig == 1, factor),
    prop_habitabilidad  = weighted.mean(habitabilidad == 1, factor),
    prop_asequibilidad  = weighted.mean(aseq == 1, factor),
    prop_tenencia       = weighted.mean(ten_f == 1, factor),
    prop_disponibilidad = weighted.mean(disponibilidad == 1, factor),
    .groups = "drop"
  ) |>
  mutate(rural = ifelse(rural == 1, "Rural", "Urbano"))
