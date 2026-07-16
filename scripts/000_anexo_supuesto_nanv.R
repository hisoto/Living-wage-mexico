#_______________________________________________________________________________

# Título: ANEXO — Supuesto fundamental del componente NANV

# Objetivo:

  # Sustentar el supuesto del componente NANV del Salario Digno:
  # "Un nivel adecuado de gasto NANV es aquel que en promedio es realizado
  #  por aquellos hogares en el nivel digno de gasto en vivienda."
  #
  # Para ello se prueba la hipótesis económica de Anker & Anker:
  # "In contrast (to food), housing and NFNH spending typically increase with
  #  income in such a way that they are not inferior goods in terms of income
  #  elasticity."
  #
  # Se estima la elasticidad-ingreso del gasto en alimentos, vivienda y NANV
  # con tres métodos complementarios:
  #   1) Log-Log survey-weighted: log(gasto_j_p) ~ log(Y_p) → η directa
  #   2) Working-Leser:           w_j ~ log(Y_p) → η = 1 + β/ŵ_j
  #   3) Engel no paramétrica:    SCAM con spline monotónico (viv, NANV) /
  #                               spline libre (alimentos)

# Autor:

  # Coordinación de Análisis de la Economía Laboral
  # de la Comisión Nacional de los Salarios Mínimos

# inputs: ENIGH 2024 (concentradohogar)
# outputs:
#   finaldata/anexo/elasticidades.csv
#   graphs/anexo/engel_alimentos.png
#   graphs/anexo/engel_vivienda.png
#   graphs/anexo/engel_nanv.png
#   graphs/anexo/elasticidades_forest.png
#   graphs/anexo/elasticidades_forest_alimentos.png
#   graphs/anexo/elasticidades_forest_vivienda.png
#   graphs/anexo/elasticidades_forest_nanv.png
#   graphs/anexo/working_leser_shares.png
#   graphs/anexo/hogares_nanv_bienestar_indicadores.png
#   graphs/anexo/hogares_nanv_bienestar_indice.png
#_______________________________________________________________________________

rm(list = ls()); gc()
options(scipen = 999)

pacman::p_load(
  tidyverse,
  data.table,
  haven,
  survey,
  srvyr,
  scam,
  Hmisc,
  statar,
  patchwork,
  broom
)

source("scripts/theme_conasami_dt2026.R")

# ── Carga y preparación de datos ─────────────────────────────────────────────

concentrado <- read_dta(
  "data/enigh2024_ns_concentradohogar_dta/concentradohogar.dta"
)

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
    nanv        = gasto_mon - alimentos - viv,
    alimentos_p = alimentos   / tot_integ,
    viv_p       = viv         / tot_integ,
    nanv_p      = nanv        / tot_integ,
    gasto_mon_p = gasto_mon   / tot_integ,
    ing_cor_p   = ing_cor     / tot_integ
  ) |>
  filter(
    alimentos  > 0,
    viv        > 0,
    nanv       > 0,
    gasto_mon  > 0,
    ing_cor    > 0
  )

# Quedan 76, 573, originales 91,414

# Recorte de outliers por ámbito (p1–p99 del gasto y el ingreso)

# 75,305 

concentrado <- concentrado |>
  group_by(rural) |>
  mutate(
    p_gasto = xtile(gasto_mon_p, n = 100, wt = factor),
    p_ing   = xtile(ing_cor_p,   n = 100, wt = factor)
  ) |>
  filter(p_gasto >= 1, p_gasto <= 99, p_ing >= 1, p_ing <= 99) |>
  select(-p_gasto, -p_ing) |>
  ungroup()

# Percentil acumulado ponderado del gasto total (para curvas Engel SCAM)
concentrado <- concentrado |>
  group_by(rural) |>
  mutate(
    gasto_p_cum_na = wtd.rank(gasto_mon_p, factor) / sum(factor)
  ) |>
  ungroup()

# ── Diseño muestral ENIGH ────────────────────────────────────────────────────

diseno <- concentrado |>
  as_survey_design(
    weights = factor,
    strata  = est_dis,
    ids     = upm
  )

options(survey.lonely.psu = "adjust")

# ── Método 1: Log-Log survey-weighted (elasticidad directa) ──────────────────

categorias <- c("alimentos" = "alimentos_p",
                "vivienda"  = "viv_p",
                "NANV"      = "nanv_p")

regresores <- c("gasto_mon" = "gasto_mon_p",
                "ing_cor"   = "ing_cor_p")

ambitos    <- c("Nacional" = NA, "Urbano" = 0, "Rural" = 1)

grid_ll <- expand_grid(
  categoria = names(categorias),
  regresor  = names(regresores),
  ambito    = names(ambitos)
)

fit_loglog <- function(categoria, regresor, ambito) {
  y_var <- categorias[[categoria]]
  x_var <- regresores[[regresor]]
  amb   <- ambitos[[ambito]]

  d <- if (is.na(amb)) diseno else diseno |> filter(rural == amb)

  fml <- as.formula(paste0("log(", y_var, ") ~ log(", x_var, ")"))
  mod <- svyglm(fml, design = d)

  tidy(mod, conf.int = TRUE) |>
    filter(term != "(Intercept)") |>
    transmute(
      metodo      = "Log-Log",
      categoria   = categoria,
      regresor    = regresor,
      ambito      = ambito,
      elasticidad = estimate,
      std_error   = std.error,
      ci_low      = conf.low,
      ci_high     = conf.high,
      p_value     = p.value,
      n_obs       = nrow(d$variables)
    )
}

elast_loglog <- pmap_dfr(grid_ll, fit_loglog)

# ── Método 2: Working-Leser (w_j ~ log(gasto_mon_p)) ─────────────────────────

# Se usa únicamente gasto_mon como regresor (Working-Leser clásico usa gasto
# corriente total).

diseno <- diseno |>
  mutate(
    w_alimentos = alimentos_p / gasto_mon_p,
    w_vivienda  = viv_p       / gasto_mon_p,
    w_NANV      = nanv_p      / gasto_mon_p
  )

shares <- c("alimentos" = "w_alimentos",
            "vivienda"  = "w_vivienda",
            "NANV"      = "w_NANV")

grid_wl <- expand_grid(
  categoria = names(shares),
  ambito    = names(ambitos)
)

fit_working_leser <- function(categoria, ambito) {
  w_var <- shares[[categoria]]
  amb   <- ambitos[[ambito]]

  d <- if (is.na(amb)) diseno else diseno |> filter(rural == amb)

  fml     <- as.formula(paste0(w_var, " ~ log(gasto_mon_p)"))
  mod     <- svyglm(fml, design = d)
  beta    <- coef(mod)[["log(gasto_mon_p)"]]
  se_beta <- sqrt(diag(vcov(mod)))[["log(gasto_mon_p)"]]

  # Media ponderada del share (para derivar la elasticidad)
  w_mean <- svymean(as.formula(paste0("~", w_var)), d) |> as.numeric()

  # Método delta para el error estándar de η = 1 + β / w̄
  #  Var(η) ≈ (1/w̄)^2 · Var(β)  (se trata w̄ como constante poblacional)
  eta    <- 1 + beta / w_mean
  se_eta <- abs(se_beta / w_mean)

  tibble(
    metodo      = "Working-Leser",
    categoria   = categoria,
    regresor    = "gasto_mon",
    ambito      = ambito,
    elasticidad = eta,
    std_error   = se_eta,
    ci_low      = eta - 1.96 * se_eta,
    ci_high     = eta + 1.96 * se_eta,
    p_value     = 2 * pnorm(-abs((eta - 0) / se_eta)),
    n_obs       = nrow(d$variables)
  )
}

elast_wl <- pmap_dfr(grid_wl, fit_working_leser)

# ── Consolidación de elasticidades ───────────────────────────────────────────

elasticidades <- bind_rows(elast_loglog, elast_wl) |>
  arrange(metodo, categoria, regresor, ambito)

fwrite(elasticidades, "finaldata/anexo/elasticidades.csv")

# ── Método 3: Engel no paramétrica (SCAM) ────────────────────────────────────

ajustar_scam <- function(datos, y_var, monotonica) {
  bs  <- if (monotonica) "mpi" else "ps"
  fml <- as.formula(paste0(y_var, " ~ s(gasto_p_cum_na, bs = '", bs, "')"))
  scam(fml, data = datos)
}

engel_df <- expand_grid(
  categoria = names(categorias),
  ambito    = c("Urbano", "Rural")
) |>
  mutate(
    y_var      = categorias[categoria],
    monotonica = categoria %in% c("vivienda", "NANV"),
    datos      = map(ambito, \(a) {
      concentrado |>
        filter(rural == ifelse(a == "Urbano", 0, 1)) |>
        arrange(gasto_p_cum_na)
    }),
    modelo = pmap(list(datos, y_var, monotonica), ajustar_scam),
    pred   = pmap(list(datos, modelo, y_var), \(d, m, y) {
      d |>
        mutate(pred = as.numeric(predict.scam(m, newdata = d))) |>
        select(gasto_p_cum_na, all_of(y), pred) |>
        rename(valor = all_of(y))
    })
  )

# Grid 3 × 2 de curvas Engel
plots_engel <- engel_df |>
  mutate(
    g = pmap(list(pred, categoria, ambito), \(df, cat, amb) {
      ggplot(df, aes(x = gasto_p_cum_na)) +
        geom_point(aes(y = valor), color = conasami_neutros[["gris"]],
                   alpha = 0.15, size = 0.6) +
        geom_line(aes(y = pred), color = conasami_colores[["guinda"]],
                  linewidth = 1.1) +
        labs(title = paste0(cat, " — ", amb)) +
        theme_conasami() +
        theme(plot.title = element_text(family = "Noto Sans", face = "bold",
                                        size = 10, hjust = 0.5,
                                        color = conasami_colores[["guinda_profundo"]]))
    })
  )

walk(c("alimentos", "vivienda", "NANV"), \(cat) {
  gs <- plots_engel |> filter(categoria == cat) |> pull(g)
  p  <- wrap_plots(gs, ncol = 2)
  guardar_grafica_conasami(p, paste0("engel_", tolower(cat)),
                           tamano = "libre", width = 17.5, height = 11,
                           dir = "graphs/anexo")
})

# ── Gráfica: Forest plot de elasticidades (Log-Log + Working-Leser) ──────────

forest_data <- bind_rows(elast_loglog, elast_wl) |>
  mutate(
    categoria = factor(categoria, levels = c("alimentos", "vivienda", "NANV")),
    ambito    = factor(ambito,    levels = c("Nacional", "Urbano", "Rural")),
    metodo    = factor(metodo,    levels = c("Log-Log", "Working-Leser"))
  )

walk(c("alimentos", "vivienda", "NANV"), \(cat) {
  g <- ggplot(
      forest_data |> filter(categoria == cat),
      aes(x = elasticidad, y = ambito, color = regresor, shape = metodo)
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = conasami_neutros[["gris"]]) +
    geom_vline(xintercept = 1, linetype = "dotted", color = conasami_neutros[["gris"]]) +
    geom_errorbarh(
      aes(xmin = ci_low, xmax = ci_high,
          group = interaction(regresor, metodo)),
      height = 0.25,
      position = position_dodge(width = 0.6)
    ) +
    geom_point(
      aes(group = interaction(regresor, metodo)),
      size     = 2.5,
      position = position_dodge(width = 0.6)
    ) +
    scale_color_manual(values = c("gasto_mon" = unname(conasami_colores[["guinda_profundo"]]),
                                  "ing_cor"   = unname(conasami_colores[["verde"]]))) +
    scale_shape_manual(values = c("Log-Log" = 16, "Working-Leser" = 17)) +
    labs(color = "Regresor", shape = "Método") +
    theme_conasami()
  guardar_grafica_conasami(g, paste0("elasticidades_forest_", tolower(cat)),
                           tamano = "ancho", dir = "graphs/anexo")
})

g_forest <- ggplot(
    forest_data,
    aes(x = elasticidad, y = ambito, color = regresor, shape = metodo)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = conasami_neutros[["gris"]]) +
  geom_vline(xintercept = 1, linetype = "dotted", color = conasami_neutros[["gris"]]) +
  geom_errorbarh(
    aes(xmin = ci_low, xmax = ci_high,
        group = interaction(regresor, metodo)),
    height = 0.25,
    position = position_dodge(width = 0.6)
  ) +
  geom_point(
    aes(group = interaction(regresor, metodo)),
    size     = 2.5,
    position = position_dodge(width = 0.6)
  ) +
  facet_wrap(~ categoria, ncol = 1) +
  scale_color_manual(
    values = c("gasto_mon" = unname(conasami_colores[["guinda_profundo"]]),
               "ing_cor"   = unname(conasami_colores[["verde"]])),
    labels = c("gasto_mon" = "Gasto monetario", "ing_cor" = "Ingreso corriente")
  ) +
  scale_shape_manual(values = c("Log-Log" = 16, "Working-Leser" = 17)) +
  labs(color = "Regresor", shape = "Método") +
  theme_conasami()

guardar_grafica_conasami(g_forest, "elasticidades_forest", tamano = "libre",
                         width = 17.5, height = 14, dir = "graphs/anexo")

# ── Gráfica: shares de Working-Leser ─────────────────────────────────────────

wl_df <- concentrado |>
  transmute(
    rural,
    ambito      = ifelse(rural == 0, "Urbano", "Rural"),
    log_gasto   = log(gasto_mon_p),
    alimentos   = alimentos_p / gasto_mon_p,
    vivienda    = viv_p       / gasto_mon_p,
    NANV        = nanv_p      / gasto_mon_p,
    factor
  ) |>
  pivot_longer(
    cols      = c(alimentos, vivienda, NANV),
    names_to  = "categoria",
    values_to = "share"
  ) |>
  mutate(
    categoria = factor(categoria, levels = c("alimentos", "vivienda", "NANV"))
  )

g_wl <- ggplot(wl_df, aes(x = log_gasto, y = share, weight = factor)) +
  geom_point(color = conasami_neutros[["gris"]], alpha = 0.1, size = 0.5) +
  geom_smooth(method = "lm", color = conasami_colores[["guinda"]],
              se = FALSE, linewidth = 1) +
  facet_grid(ambito ~ categoria, scales = "free_y") +
  theme_conasami()

guardar_grafica_conasami(g_wl, "working_leser_shares", tamano = "libre",
                         width = 17.5, height = 12, dir = "graphs/anexo")

# ── Verificación económica ───────────────────────────────────────────────────

cat("\n================================================================\n")
cat("DIAGNÓSTICO DEL SUPUESTO FUNDAMENTAL DEL COMPONENTE NANV\n")
cat("================================================================\n")
cat("Hipótesis: η_vivienda >= 0 y η_NANV >= 0 en los tres ámbitos\n")
cat("           (no son bienes inferiores en elasticidad-ingreso).\n")
cat("Contraste: η_alimentos típicamente < 1 (necesidad; Ley de Engel).\n\n")

resumen <- elasticidades |>
  group_by(metodo, categoria) |>
  summarise(
    eta_prom = mean(elasticidad),
    eta_min  = min(elasticidad),
    eta_max  = max(elasticidad),
    .groups  = "drop"
  )

print(resumen)

cat("\nTest de hipótesis (IC 95% excluye 0):\n")
test_hip <- elast_loglog |>
  mutate(no_inferior = ci_low > 0) |>
  group_by(categoria) |>
  summarise(
    prop_no_inferior = mean(no_inferior),
    .groups          = "drop"
  )
print(test_hip)

cat("\nOutputs guardados en:\n")
cat("  - finaldata/anexo/elasticidades.csv\n")
cat("  - graphs/anexo/engel_alimentos.png\n")
cat("  - graphs/anexo/engel_vivienda.png\n")
cat("  - graphs/anexo/engel_nanv.png\n")
cat("  - graphs/anexo/elasticidades_forest.png\n")
cat("  - graphs/anexo/elasticidades_forest_alimentos.png\n")
cat("  - graphs/anexo/elasticidades_forest_vivienda.png\n")
cat("  - graphs/anexo/elasticidades_forest_nanv.png\n")
cat("  - graphs/anexo/working_leser_shares.png\n")

#_______________________________________________________________________________

# ── ANEXO B: Caracterización de hogares-NANV por Indicadores de Bienestar ────

# Objetivo:
#   Sustentar que los hogares usados para calcular el gasto medio NANV en
#   scripts/3.canasta_nanv.R (banda [inverse_result, inverse_result + 0.1]
#   del percentil acumulado del gasto corriente) corresponden efectivamente
#   a hogares en nivel digno según los Indicadores de Bienestar definidos
#   en scripts/1.canasta_alimentos.R.
#
# Se comparan tres grupos:
#   1) poblacion_total   — todos los hogares ENIGH 2024.
#   2) estrato_ref_alim  — estrato de referencia del componente alimentos
#                          (urbano p41–p61 / rural p80–p100 del ingreso p.c.).
#   3) hogares_nanv      — unión de los 66 subconjuntos (32 entidades × 2
#                          ámbitos + 2 ámbitos nacionales) usados para
#                          estimar el gasto medio NANV.

# ── B.1 Construcción de Indicadores de Bienestar (replica script 1) ──────────

viviendas_ind <- read_dta("data/enigh2024_ns_viviendas_dta/viviendas.dta") |>
  mutate(
    across(
      where(is.character) & !matches("folioviv"),
      ~ suppressWarnings(as.numeric(.))
    ),
    rural            = ifelse(substr(folioviv, 3, 3) == "6", 1, 0),
    habitan_p_cuarto = tot_resid / cuart_dorm,
    agua_e  = ifelse(agua_ent == 1 | agua_ent == 2, 1, 0),
    agua_ab = ifelse(ab_agua >= 1 &
                       ab_agua <= 4 |
                       ((ab_agua == 6 &
                           procaptar == 1) |
                          (agua_noe == 5) | (agua_noe == 6 & procaptar == 1)
                       ), 1, 0),
    agua_d  = ifelse(dotac_agua == 1 | dotac_agua == 2, 1, 0),
    paredes = ifelse((mat_pared == 8) |
                       (mat_pared == 7 & rural == 1), 1, 0),
    techos  = ifelse(mat_techos == 10 | mat_techos == 9, 1, 0),
    pisos   = ifelse((mat_pisos == 2 | mat_pisos == 3), 1, 0),
    probms  = ifelse((p_pandeos == 1 | p_levanta == 1 |
                        p_humedad == 1 | p_fractura == 1), 0, 1),
    wc      = ifelse(excusado == 1 | excusado == 2, 1, 0),
    twc     = ifelse(uso_compar == 2, 1, 0),
    awc     = ifelse(sanit_agua == 1, 1, 0),
    red     = ifelse(drenaje == 1, 1, 0)
  ) |>
  mutate(
    i.agua         = ifelse(agua_e == 1 & agua_ab == 1 & agua_d == 1, 1, 0),
    i.materiales   = ifelse(paredes == 1 & techos == 1 &
                              pisos == 1 & probms == 1, 1, 0),
    i.hacinamiento = ifelse(habitan_p_cuarto <= 2.5, 1, 0),
    i.saneamiento  = ifelse(wc == 1 & twc == 1 & awc == 1 & red == 1, 1, 0)
  ) |>
  select(folioviv, starts_with("i."))

poblacion_ind <- read_dta("data/enigh2024_ns_poblacion_dta/poblacion.dta") |>
  mutate(
    id.sin_edu_ini = ifelse(edad < 3 & edu_ini == 6 &
                              no_asis %in% c(3, 5), 1, 0),
    id.sis_edu     = ifelse(edad >= 3 & edad <= 18 & asis_esc == 2, 1, 0)
  ) |>
  group_by(folioviv, foliohog) |>
  summarise(i.edu = ifelse(any(id.sin_edu_ini == 1 | id.sis_edu == 1), 0, 1),
            .groups = "drop")

hogares_ind <- read_dta("data/enigh2024_ns_hogares_dta/hogares.dta") |>
  select(-c(acc_alim18)) |>
  mutate(i.alim = if_else(if_any(starts_with("acc_alim"), ~ .x == 1), 0, 1)) |>
  select(folioviv, foliohog, i.alim)

indicadores <- full_join(viviendas_ind, poblacion_ind, by = "folioviv") |>
  full_join(hogares_ind, by = c("folioviv", "foliohog"))

rm(viviendas_ind, poblacion_ind, hogares_ind)

# Se carga concentradohogar fresco (sin los filtros del anexo de elasticidades)

concentrado_ind <- read_dta(
  "data/enigh2024_ns_concentradohogar_dta/concentradohogar.dta"
) |>
  left_join(indicadores, by = c("folioviv", "foliohog")) |>
  mutate(
    rural           = ifelse(substr(folioviv, 3, 3) == "6", 1L, 0L),
    cve_ent         = as.numeric(substr(folioviv, 1, 2)),
    nom_ent         = case_when(
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
    i.exceso.g.alim = ifelse(alimentos / gasto_mon >= .75, 0, 1),
    indice          = i.saneamiento + i.materiales + i.edu + i.alim
  ) |>
  rename(factor_exp = factor)

percentiles_nanv <- read_csv(
  "finaldata/nanv/2.percentiles_nanv.csv"
)

concentrado_ind <- concentrado_ind |> 
  left_join(percentiles_nanv, by = c("cve_ent", "nom_ent", "rural"))


rm(indicadores, percentiles_nanv)

# ── B.2 Estrato de referencia de alimentos (replica script 1) ────────────────

concentrado_ind <- concentrado_ind |>
  mutate(ing_p = ing_cor / tot_integ) |>
  group_by(rural) |>
  mutate(percentil_ing = xtile(ing_p, n = 100, wt = factor_exp)) |>
  ungroup() |>
  mutate(
    estrato_ref_alim = (rural == 0 & percentil_ing >= 41 & percentil_ing <= 61) |
                      (rural == 1 & percentil_ing >= 80 & percentil_ing <= 100)
  )

# ── B.3 Selección de hogares-NANV (replica script 3) ─────────────────────────

# Se replica el filtrado y el cálculo del percentil acumulado del gasto
# corriente per cápita tal como en scripts/3.canasta_nanv.R.

concentrado_nanv <- concentrado_ind |>
  mutate(
    viv         = vivienda + estim_alqu,
    viv_p       = viv       / tot_integ,
    gasto_mon_p = gasto_mon / tot_integ
  ) |>
  filter(viv != 0) |>
  arrange(gasto_mon) |>
  group_by(nom_ent, rural) |>
  mutate(percentil = xtile(gasto_mon, n = 100, wt = factor_exp)) |>
  filter(percentil >= 5, percentil <= 95) |>
  ungroup() |>
  group_by(nom_ent) |>
  mutate(gasto_p_cum = wtd.rank(gasto_mon_p, factor_exp) / sum(factor_exp)) |>
  ungroup() |>
  group_by(rural) |>
  mutate(gasto_p_cum_na = wtd.rank(gasto_mon_p, factor_exp) / sum(factor_exp)) |>
  ungroup() |>
  filter(!is.na(gasto_p_cum), !is.na(gasto_p_cum_na), !is.na(viv_p))

# Vivienda digna per cápita por entidad y ámbito (pred es por hogar de 4)
canasta_vivienda_anexo <- fread("finaldata/vivienda/canasta_vivienda.csv") |>
  mutate(
    pred_p  = pred / 4,
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
      cve_ent == 32 ~ "Zacatecas",
      cve_ent == 33 ~ "Nacional"
    )
  )

# Función que ajusta SCAM monotónico creciente y devuelve, por hogar,
# si pertenece a la banda NANV [inv, inv + 0.1] (grupo = "nanv") o si
# está por debajo del umbral inferior (grupo = "menor").
seleccion_nanv <- function(datos, x_var, pred_value, banda = 0.1) {
  empty <- tibble(folioviv = character(),
                  foliohog = integer(),
                  grupo    = character())
  if (nrow(datos) < 30) return(empty)

  mod <- tryCatch(
    scam(as.formula(paste0("viv_p ~ s(", x_var, ", bs = 'mpi')")),
         data = datos),
    error = function(e) NULL
  )
  if (is.null(mod)) return(empty)

  x_range <- range(datos[[x_var]], na.rm = TRUE)

  inv <- tryCatch(
    uniroot(
      function(x) {
        nd <- data.frame(x)
        names(nd) <- x_var
        as.numeric(predict.scam(mod, newdata = nd)) - pred_value
      },
      lower = x_range[1],
      upper = x_range[2]
    )$root,
    error = function(e) NA_real_
  )
  if (is.na(inv)) return(empty)

  bind_rows(
    datos |>
      filter(.data[[x_var]] >= inv & .data[[x_var]] <= inv + banda) |>
      select(folioviv, foliohog) |>
      distinct() |>
      mutate(grupo = "nanv"),
    datos |>
      filter(.data[[x_var]] < inv) |>
      select(folioviv, foliohog) |>
      distinct() |>
      mutate(grupo = "menor")
  )
}

# Loop: 32 entidades × 2 ámbitos (x_var = gasto_p_cum)
hogares_estados <- expand_grid(
  nom_ent = setdiff(unique(concentrado_nanv$nom_ent), NA),
  rural   = c(0, 1)
) |>
  mutate(
    pred_p = map2_dbl(nom_ent, rural, \(e, r) {
      canasta_vivienda_anexo |>
        filter(nom_ent == e, rural == r) |>
        pull(pred_p) |>
        (\(x) if (length(x) == 0) NA_real_ else x[1])()
    }),
    hogares = pmap(list(nom_ent, rural, pred_p), \(e, r, p) {
      if (is.na(p)) return(tibble(folioviv = character(),
                                  foliohog = integer(),
                                  grupo    = character()))
      cat("NANV selección —", e, ifelse(r == 1, "rural", "urbano"), "\n")
      datos_g <- concentrado_nanv |>
        filter(nom_ent == e, rural == r) |>
        arrange(gasto_p_cum)
      seleccion_nanv(datos_g, "gasto_p_cum", p)
    })
  ) |>
  select(nom_ent, rural, hogares) |>
  unnest(hogares)

# Loop: 2 ámbitos nacionales (x_var = gasto_p_cum_na)
hogares_nacional <- tibble(rural = c(0, 1)) |>
  mutate(
    pred_p = map_dbl(rural, \(r) {
      canasta_vivienda_anexo |>
        filter(nom_ent == "Nacional", rural == r) |>
        pull(pred_p) |>
        (\(x) if (length(x) == 0) NA_real_ else x[1])()
    }),
    hogares = map2(rural, pred_p, \(r, p) {
      if (is.na(p)) return(tibble(folioviv = character(),
                                  foliohog = integer(),
                                  grupo    = character()))
      cat("NANV selección — Nacional", ifelse(r == 1, "rural", "urbano"), "\n")
      datos_g <- concentrado_nanv |>
        filter(rural == r) |>
        arrange(gasto_p_cum_na)
      seleccion_nanv(datos_g, "gasto_p_cum_na", p)
    })
  ) |>
  select(rural, hogares) |>
  unnest(hogares)

flags_hogares <- bind_rows(
  hogares_estados  |> select(folioviv, foliohog, grupo),
  hogares_nacional |> select(folioviv, foliohog, grupo)
) |>
  distinct() |>
  group_by(folioviv, foliohog) |>
  summarise(
    flag_nanv      = any(grupo == "nanv"),
    flag_pct_menor = any(grupo == "menor"),
    .groups        = "drop"
  ) |>
  # Mutua exclusión: si un hogar califica como NANV en algún subgrupo,
  # se cuenta como NANV y se excluye de "percentiles menores".
  mutate(flag_pct_menor = flag_pct_menor & !flag_nanv)

concentrado_ind <- concentrado_ind |>
  left_join(flags_hogares, by = c("folioviv", "foliohog")) |>
  mutate(
    flag_nanv      = coalesce(flag_nanv,      FALSE),
    flag_pct_menor = coalesce(flag_pct_menor, FALSE)
  )

rm(concentrado_nanv, hogares_estados, hogares_nacional, flags_hogares)

# ── B.4 % cumplimiento de indicadores por grupo ──────────────────────────────

resumen_grupo <- function(df) {
  d <- df |>
    filter(!is.na(i.agua), !is.na(i.materiales), !is.na(i.hacinamiento),
           !is.na(i.saneamiento), !is.na(i.edu), !is.na(i.alim),
           !is.na(i.exceso.g.alim), !is.na(indice))

  w <- d$factor_exp
  wmean <- function(x) sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)

  tibble(
    n_hogares_ponderado = sum(w, na.rm = TRUE),
    pct_i_agua          = 100 * wmean(d$i.agua),
    pct_i_materiales    = 100 * wmean(d$i.materiales),
    pct_i_hacinamiento  = 100 * wmean(d$i.hacinamiento),
    pct_i_saneamiento   = 100 * wmean(d$i.saneamiento),
    pct_i_edu           = 100 * wmean(d$i.edu),
    pct_i_alim          = 100 * wmean(d$i.alim),
    pct_i_exceso_g_alim = 100 * wmean(d$i.exceso.g.alim),
    indice_0            = 100 * wmean(d$indice == 0),
    indice_1            = 100 * wmean(d$indice == 1),
    indice_2            = 100 * wmean(d$indice == 2),
    indice_3            = 100 * wmean(d$indice == 3),
    indice_4            = 100 * wmean(d$indice == 4),
    indice_promedio     = wmean(d$indice)
  )
}

tabla_bienestar <- bind_rows(
  resumen_grupo(concentrado_ind |> filter(flag_pct_menor)) |>
    mutate(grupo = "percentiles_menores_nanv", .before = 1),
  resumen_grupo(concentrado_ind |> filter(estrato_ref_alim)) |>
    mutate(grupo = "estrato_ref_alim", .before = 1),
  resumen_grupo(concentrado_ind |> filter(flag_nanv)) |>
    mutate(grupo = "hogares_nanv", .before = 1)
)

fwrite(tabla_bienestar, "finaldata/anexo/hogares_nanv_bienestar.csv")

# ── B.5 Gráfica comparativa ──────────────────────────────────────────────────

niveles_grupo <- c("percentiles_menores_nanv", "estrato_ref_alim", "hogares_nanv")
etiquetas_grupo <- c(
  "percentiles_menores_nanv" = "Percentiles < NANV",
  "estrato_ref_alim"         = "Estrato ref. alimentos",
  "hogares_nanv"             = "Hogares NANV"
)
colores_grupo <- c(
  "percentiles_menores_nanv" = unname(conasami_neutros[["gris"]]),
  "estrato_ref_alim"         = unname(conasami_colores[["verde"]]),
  "hogares_nanv"             = unname(conasami_colores[["guinda_profundo"]])
)

niveles_indicador <- c(
  "pct_i_agua"          = "Agua",
  "pct_i_materiales"    = "Materiales",
  "pct_i_hacinamiento"  = "Sin hacinamiento",
  "pct_i_saneamiento"   = "Saneamiento",
  "pct_i_edu"           = "Educación",
  "pct_i_alim"          = "Sin carencia alim.",
  "pct_i_exceso_g_alim" = "Sin exceso gasto alim."
)

panel_a_df <- tabla_bienestar |>
  select(grupo, all_of(names(niveles_indicador))) |>
  pivot_longer(-grupo, names_to = "indicador", values_to = "pct") |>
  mutate(
    grupo     = factor(grupo, levels = niveles_grupo,
                       labels = etiquetas_grupo[niveles_grupo]),
    indicador = factor(indicador, levels = names(niveles_indicador),
                       labels = niveles_indicador[names(niveles_indicador)])
  )

g_panel_a <- ggplot(panel_a_df,
                    aes(x = indicador, y = pct, fill = grupo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(
    aes(label = sprintf("%.1f%%", pct)),
    position = position_dodge(width = 0.8),
    vjust    = -0.4,
    size     = 2,
    color    = conasami_neutros[["tinta"]],
    fontface = "bold"
  ) +
  scale_fill_manual(values = setNames(colores_grupo[niveles_grupo],
                                      etiquetas_grupo[niveles_grupo])) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 20)) +
  labs(fill = NULL) +
  theme_conasami() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

panel_b_df <- tabla_bienestar |>
  select(grupo, indice_0, indice_1, indice_2, indice_3, indice_4) |>
  pivot_longer(-grupo, names_to = "nivel", values_to = "pct") |>
  mutate(
    grupo = factor(grupo, levels = niveles_grupo,
                   labels = etiquetas_grupo[niveles_grupo]),
    nivel = factor(nivel,
                   levels = c("indice_0", "indice_1", "indice_2",
                              "indice_3", "indice_4"),
                   labels = c("0", "1", "2", "3", "4"))
  )

g_panel_b <- ggplot(panel_b_df, aes(x = grupo, y = pct, fill = nivel)) +
  geom_col(position = "stack", width = 0.65) +
  geom_text(
    aes(label = ifelse(pct >= 3, sprintf("%.1f", pct), "")),
    position = position_stack(vjust = 0.5),
    size     = 2.8,
    color    = "white"
  ) +
  scale_fill_manual(
    values = c("0" = unname(conasami_colores[["guinda_profundo"]]),
               "1" = unname(conasami_neutros[["gris"]]),
               "2" = unname(conasami_colores[["dorado"]]),
               "3" = unname(conasami_colores[["verde"]]),
               "4" = unname(conasami_colores[["verde_profundo"]]))
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(fill = "Indicadores\ncumplidos") +
  theme_conasami()

guardar_grafica_conasami(g_panel_a, "hogares_nanv_bienestar_indicadores",
                         tamano = "libre", width = 17.5, height = 9,
                         dir = "graphs/anexo")

guardar_grafica_conasami(g_panel_b, "hogares_nanv_bienestar_indice",
                         tamano = "libre", width = 12, height = 8,
                         dir = "graphs/anexo")

# ── B.6 Resumen en consola ───────────────────────────────────────────────────

cat("\n================================================================\n")
cat("CARACTERIZACIÓN DE HOGARES-NANV POR INDICADORES DE BIENESTAR\n")
cat("================================================================\n")
cat("Grupos comparados (ponderado por factor_exp):\n")
cat("  · percentiles_menores_nanv — hogares con gasto_p_cum < inv (umbral inferior NANV) en su subgrupo\n")
cat("  · estrato_ref_alim         — urbano p41–p61 / rural p80–p100 de ing. p.c.\n")
cat("  · hogares_nanv             — unión de las 66 selecciones del script 3\n\n")

print(
  tabla_bienestar |>
    mutate(across(where(is.numeric), \(x) round(x, 2)))
)

cat("\nOutputs guardados en:\n")
cat("  - finaldata/anexo/hogares_nanv_bienestar.csv\n")
cat("  - graphs/anexo/hogares_nanv_bienestar_indicadores.png\n")
cat("  - graphs/anexo/hogares_nanv_bienestar_indice.png\n")
