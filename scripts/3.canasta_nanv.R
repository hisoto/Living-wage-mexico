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
  
  # Base Concentrado


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
  "tidyverse",
  "stringr",
  "survey",
  "srvyr",
  "data.table",
  "foreign",
  "extrafont",
  "scam",
  "statar",
  "Hmisc",
  "haven"
)

source("scripts/theme_conasami.R")

#_______________________________________________________________________________

concentrado <- read_dta("data/enigh2024_ns_concentradohogar_dta/concentradohogar.dta") # Base que contiene los indicadores de la ENIGH

concentrado <- concentrado %>%
  mutate(
    rural = ifelse(substr(folioviv, 3, 3) == "6", 1, 0),
    # Identificamos rural
    cve_ent = as.numeric(substr(folioviv, 1, 2)),
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
    # Creamos la variable de vivienda, gasto en vivienda + alquiler estimado
    viv = vivienda + estim_alqu,
    # Identificamos el gasto en vivienda y el corriente total por persona
    viv_p = viv / tot_integ,
    gasto_mon_p = gasto_mon / tot_integ
  ) %>%
  # Eliminamos a todos los hogares que no reportaran gasto
  filter(viv != 0)
#_______________________________________________________________________________

# Quitamos los outliers por entidad

concentrado <- concentrado %>%
  arrange(gasto_mon) %>%
  group_by(nom_ent, rural) %>%
  mutate(percentil = xtile(gasto_mon, n = 100, wt = factor))  %>%
  filter(percentil >= 5 & percentil <= 95) %>%
  ungroup() %>%
  group_by(nom_ent) %>%
  mutate(
    gasto_cum = wtd.rank(gasto_mon, factor) / sum(factor),
    gasto_p_cum = wtd.rank(gasto_mon_p, factor) / sum(factor)
  ) %>%
  filter(!is.na(gasto_cum), !is.na(viv_p), !is.na(gasto_p_cum)) %>%
  ungroup() %>%
  mutate(
    gasto_cum_na = wtd.rank(gasto_mon, factor) / sum(factor),
    gasto_p_cum_na = wtd.rank(gasto_mon_p, factor) / sum(factor)
  ) %>%
  filter(!is.na(gasto_p_cum_na), !is.na(gasto_cum_na))

#_______________________________________________________________________________

resultados <- list()

# Para cada ambito se realiza:
for (ambito in c(0, 1)) {
  cat("Procesando:", ambito, "\n")
  
  # Se filtran los datos
  datos_ambito <- concentrado %>%
    filter(rural == ambito) %>%
    arrange(gasto_p_cum_na)
  
  # Se crea el modelo con la paqueteria scam
  modelo_ambito <- scam(viv_p ~ s(gasto_p_cum_na, bs = "mpi"), data = datos_ambito)
  
  # Se generan las predicciones
  datos_ambito <- datos_ambito %>%
    mutate(pred = predict.scam(modelo_ambito))
  
  ggplot(datos_ambito) +
    geom_point(aes(x = gasto_p_cum_na, y = viv_p), color = "#611232") +
    geom_smooth(
      aes(x = gasto_p_cum_na, y = viv_p),
      method = "loess",
      span = .5,
      se = FALSE
    ) +
    geom_line(aes(x = gasto_p_cum_na, y = pred),
              color = "red",
              linewidth = 1) +
    labs(title = "", x = "Probabilidad acumulada del Gasto Corriente", y = "Alquiler per cápita") +
    theme_conasami() +
    theme(
      plot.title = element_text(hjust = 0.5, color = "white"),
      axis.title = element_text(size = 10, color = "white"),
      text = element_text(family = "Noto Sans", color = "white"),
      axis.text = element_text(color = "white")
        )
  
  ggsave(
    filename = paste0(
      "graphs/relacion/nacional/",
      ifelse(ambito == 0, "urbano", "rural"),
      "_nacional.png"
    ),
    plot = last_plot(),
    width = 10,
    height = 6
  )
  
  # Se genera un local dentro de la función for
  local({
    current_model <- modelo_ambito
    current_data <- datos_ambito
    
    # Se genera una función para producir las predicciones del modelo
    f <- function(xval)
      predict.scam(current_model, newdata = data.frame(gasto_p_cum_na = xval))
    
    # Se genera la función inversa a la función anterior
    f_inv <- function(yval) {
      uniroot(
        function(x)
          f(x) - yval,
        lower = min(current_data$gasto_p_cum_na),
        upper = max(current_data$gasto_p_cum_na)
      )$root
    }
    
    # Se guardan los resultados por ambito
    resultados[[ifelse(ambito == 0, "urbano", "rural")]] <<- list(
      datos = current_data,
      modelo = current_model,
      f = f,
      f_inv = f_inv
    )
  })
  
}


rm(modelo_ambito, datos_ambito)

range(resultados$urbano$datos$pred) # Para identificar el rango de la función.

#_______________________________________________________________________________

# Por entidades

entidades <- c(
  "Aguascalientes",
  "Baja California",
  "Baja California Sur",
  "Campeche",
  "Coahuila",
  "Colima",
  "Chiapas",
  "Chihuahua",
  "Ciudad de México",
  "Durango",
  "Guanajuato",
  "Guerrero",
  "Hidalgo",
  "Jalisco",
  "México",
  "Michoacán",
  "Morelos",
  "Nayarit",
  "Nuevo León",
  "Oaxaca",
  "Puebla",
  "Querétaro",
  "Quintana Roo",
  "San Luis Potosí",
  "Sinaloa",
  "Sonora",
  "Tabasco",
  "Tamaulipas",
  "Tlaxcala",
  "Veracruz",
  "Yucatán",
  "Zacatecas"
)

#_______________________________________________________________________________

# Se repite el mismo proceso para cada entidad por ambito urbano

resultados_urbano <- list()

entidades_urbano <- concentrado %>%
  filter(rural == 0)

for (ent in entidades) {
  cat("Procesando:", ent, "\n")
  
  datos_ent <- entidades_urbano %>%
    filter(nom_ent == ent) %>%
    arrange(gasto_p_cum)
  
  modelo_ent <- scam(viv_p ~ s(gasto_p_cum, bs = "mpi"), data = datos_ent)
  
  datos_ent <- datos_ent %>%
    mutate(pred = predict.scam(modelo_ent))
  
  
  ggplot(datos_ent) +
    geom_point(aes(x = gasto_p_cum, y = viv_p), color = "#611232") +
    geom_smooth(
      aes(x = gasto_p_cum, y = viv_p),
      method = "loess",
      span = .5,
      se = FALSE
    ) +
    geom_line(aes(x = gasto_p_cum, y = pred),
              color = "red",
              linewidth = 1) +
    labs(title = ent, x = "Probabilidad acumulada del Gasto Corriente", y = "Alquiler per cápita") +
    theme_conasami() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_text(size = 10))
  
  ggsave(
    filename = paste0("graphs/relacion/urbano/", ent, ".png"),
    plot = last_plot(),
    width = 10,
    height = 6
  )
  
  local({
    current_model <- modelo_ent
    current_data <- datos_ent
    
    f <- function(xval)
      predict.scam(current_model, newdata = data.frame(gasto_p_cum = xval))
    
    f_inv <- function(yval) {
      uniroot(
        function(x)
          f(x) - yval,
        lower = min(current_data$gasto_p_cum),
        upper = max(current_data$gasto_p_cum)
      )$root
    }
    
    resultados_urbano[[ent]] <<- list(
      datos = current_data,
      modelo = current_model,
      f = f,
      f_inv = f_inv
    )
    
  })
  
}

rm(modelo_ent, datos_ent)

#_______________________________________________________________________________

# Se repite el mismo proceso por cada entidad en ambito rural

resultados_rural <- list()

entidades_rural <- concentrado %>%
  filter(rural == 1)

for (ent in entidades) {
  cat("Procesando:", ent, "\n")
  
  datos_ent <- entidades_rural %>%
    filter(nom_ent == ent) %>%
    arrange(gasto_p_cum)
  
  modelo_ent <- scam(viv_p ~ s(gasto_p_cum, bs = "mpi"), data = datos_ent)
  
  datos_ent <- datos_ent %>%
    mutate(pred = predict.scam(modelo_ent))
  
  ggplot(datos_ent) +
    geom_point(aes(x = gasto_p_cum, y = viv_p), color = "#611232") +
    geom_smooth(
      aes(x = gasto_p_cum, y = viv_p),
      method = "loess",
      span = .5,
      se = FALSE
    ) +
    geom_line(aes(x = gasto_p_cum, y = pred),
              color = "red",
              linewidth = 1) +
    labs(title = ent, x = "Probabilidad acumulada del Gasto Corriente", y = "Alquiler per cápita") +
    theme_conasami() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_text(size = 10))
  
  ggsave(
    filename = paste0("graphs/relacion/rural/", ent, "_rural.png"),
    plot = last_plot(),
    width = 10,
    height = 6
  )
  
  local({
    current_model <- modelo_ent
    current_data <- datos_ent
    
    f <- function(xval)
      predict.scam(current_model, newdata = data.frame(gasto_p_cum = xval))
    
    f_inv <- function(yval) {
      uniroot(
        function(x)
          f(x) - yval,
        lower = min(current_data$gasto_p_cum),
        upper = max(current_data$gasto_p_cum)
      )$root
    }
    
    resultados_rural[[ent]] <<- list(
      datos = current_data,
      modelo = current_model,
      f = f,
      f_inv = f_inv
    )
    
  })
  
}

rm(modelo_ent, datos_ent)

#_______________________________________________________________________________

# Se guardan los resultados

master_results <- list(nacional = resultados,
                       estados = lapply(setNames(entidades, entidades), function(ent) {
                         list(urbano = resultados_urbano[[ent]], rural = resultados_rural[[ent]])
                       }))

rm(resultados_urbano,
   resultados_rural,
   resultados,
   entidades_rural,
   entidades_urbano)

#_______________________________________________________________________________

# Se cargan los valores estimados de una vivienda digna

canasta_vivienda <- fread("finaldata/vivienda/canasta_vivienda.csv") %>%
  mutate(
    pred = pred / 4,
    # Entre cuatro para el valor individual
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
      cve_ent == 32 ~ "Zacatecas",
      cve_ent == 33 ~ "Nacional"
    )
  )

# Se genera una función para aplicar f_inv a cada entidad, ambito y a nivel nacional

safe_f_inv <- function(numeric_value, state, is_rural) {
  tryCatch({
    if (is_rural == 1) {
      # Modelo rural
      if (state == "Nacional") {
        master_results$nacional$rural$f_inv(numeric_value)
      } else {
        master_results$estados[[state]]$rural$f_inv(numeric_value)
      }
    } else {
      # Modelos Urbano
      if (state == "Nacional") {
        master_results$nacional$urbano$f_inv(numeric_value)
      } else {
        master_results$estados[[state]]$urbano$f_inv(numeric_value)
      }
    }
  }, error = function(e) {
    warning(paste(
      "Error",
      state,
      ifelse(is_rural == 1, "rural", "urbano"),
      "con valor",
      numeric_value,
      ":",
      e$message
    ))
    return(NA)
  })
}

# Se aplica la función

canasta_vivienda <- canasta_vivienda %>%
  rowwise() %>%
  mutate(inverse_result = safe_f_inv(pred, nom_ent, rural)) %>%
  ungroup()

#_______________________________________________________________________________

# Entidades

concentrado <- concentrado %>% # pegamos los resultados anteriores a concentrado
  left_join(canasta_vivienda %>%
              select(nom_ent, rural, inverse_result),
            by = c("nom_ent", "rural"))

# Se hace una lista anidada de concentrado por entidad y ambito
concentrado <- concentrado %>%
  group_by(nom_ent, rural) %>%
  nest()

# Generamos la función para obtener el valor de NANV

mean_gasto_mon_fun <- function(dat) {
  df <- dat %>%
    # Para cada entidad filtra el intervalo que comienza con la estimación de f_inv
    filter(gasto_p_cum >= inverse_result &
             gasto_p_cum <= (inverse_result + .1))
  
  # Se aplica el diseño de la encuesta
  design <- svydesign(
    id = ~ upm,
    weights = ~ factor,
    strata = ~ est_dis,
    data = df
  )
  
  # Como algunas entidades tienen pocas observaciones se ajusta el calculo de los errores estándar
  options(survey.lonely.psu = "adjust")
  
  # Se estima la media
  mean_gasto_mon <- svymean( ~ gasto_mon_p, design)
  
  return(mean_gasto_mon)
}


# Se aplica la función a la lista anidada
canv <- concentrado %>%
  mutate(mean_gasto_mon = map(data, mean_gasto_mon_fun))

# Se guarda en un df
final <- canv %>%
  select(rural, nom_ent, mean_gasto_mon) %>%
  mutate(mean_gasto_mon = map_dbl(mean_gasto_mon, ~ as.numeric(.x[1])))


#_______________________________________________________________________________

# Se repite el mismo proceso para Nacional
# Nacional

concentrado <- concentrado %>%
  # Desabidamos concentrado
  unnest(data)

p <- canasta_vivienda %>%
  filter(nom_ent == "Nacional") %>%
  select(rural, inverse_result) %>%
  rename(inverse_nac = inverse_result)

concentrado <- concentrado %>%
  left_join(p, by = c("rural"))

concentrado <- concentrado %>%
  # Anidamos concentrado por ambito
  group_by(rural) %>%
  nest()

mean_gasto_mon_fun_nac <- function(dat) {
  df <- dat %>%
    # En este caso se usa la distribución nacional
    filter(gasto_p_cum_na >= inverse_nac &
             gasto_p_cum_na <= (inverse_nac + .1))
  
  design <- svydesign(
    id = ~ upm,
    weights = ~ factor,
    strata = ~ est_dis,
    data = df
  )
  
  options(survey.lonely.psu = "adjust")
  
  mean_gasto_mon <- svymean( ~ gasto_mon_p, design)
  
  return(mean_gasto_mon)
  
}

concentrado <- concentrado %>%
  # Aplicamos la función a la lista anidada
  mutate(mean_gasto_mon = map(data, mean_gasto_mon_fun_nac))

# Creamos un df
nacional <- concentrado %>%
  select(rural, mean_gasto_mon) %>%
  mutate(mean_gasto_mon = map_dbl(mean_gasto_mon, ~ as.numeric(.x[1]))) %>%
  mutate(nom_ent = "Nacional")

# Unimos entidades y nacional
final <- final %>%
  bind_rows(nacional) %>%
  left_join(
    canasta_vivienda %>% 
      select(-c(inverse_result)), by = c("rural", "nom_ent")
    ) %>%
  rename(vivienda = pred) %>%
  arrange(desc(mean_gasto_mon)) %>%
  mutate(NANV = mean_gasto_mon / 3) %>% # se divide entre tres para obtener el valor mensual
  select(-c(mean_gasto_mon)) %>%
  relocate(nom_ent, cve_ent, rural, vivienda, NANV)

fwrite(final, "finaldata/nanv/1.canasta_nanv.csv")

################################################################################

# Aquí concluye el calculo de la NANV

################################################################################

# Graficas ---------------------------------------------------------------------

ggplot(canasta_vivienda %>% mutate(rural = as.character(rural))) +
  geom_bar(
    mapping = aes(
      x = reorder(nom_ent, inverse_result * (rural == 1)),
      y = inverse_result,
      fill = as.factor(rural)
    ),
    stat = "identity",
    position = "dodge"
  ) +
  theme_conasami() +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  scale_fill_manual(
    values = c("0" = "#611232", "1" = "#98989A"),
    labels = c("Urbano", "Rural")
  ) +
  scale_color_manual(
    values = c("0" = "#611232", "1" = "#98989A"),
    labels = c("Urbano", "Rural")
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(
  "graphs/relacion/nacional/inverse_result.png",
  width = 12,
  height = 6
)

# prueba de valores alternativos de NANV

ratio <- concentrado %>%
  unnest(data) %>%
  filter(gasto_p_cum_na >= (inverse_nac - .1) &
           gasto_p_cum_na <= (inverse_nac + .1)) %>%
  group_by() %>%
  summarise(ratio = median((gasto_mon -
                              viv -
                              alimentos) / viv)) %>%
  pull()

alternativo <- final %>%
  mutate(
    vivienda = (vivienda * 4) / 3,
    nanv_alt = ifelse((vivienda * ratio) < NANV, vivienda * ratio, NANV)
  )

#fwrite(alternativo, "finaldata/canasta_nanv_alternativo.csv")
