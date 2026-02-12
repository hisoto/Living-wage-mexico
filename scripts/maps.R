#_______________________________________________________________________________

# Coordinación de Analísis de la Economía Laboral 
# Script para gráficas del salario digno 

#_______________________________________________________________________________

rm(list = ls()); gc()

if (!require(pacman)) install.packages("pacman") 
library(pacman)

p_load("tidyverse",
       "stringr", 
       "Hmisc",
       "EnvStats",
       "survey",
       "srvyr", 
       "data.table",
       "bit64", 
       "statar", 
       "foreign", 
       "openxlsx", 
       "extrafont", 
       "readxl", 
       "patchwork",
       "rnaturalearth",
       "rnaturalearthhires",
       "haven")

source("scripts/theme_conasami.R")

#install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev", type = "source")

#_______________________________________________________________________________

living_wage <- fread("salario_digno.csv") %>% 
  rename(salario_digno_neto = salario_digno,
         alimentos_individual = canasta_alimentos,
         nanv = NANV,
         vivienda_familiar = vivienda_mensual)

# gráfica de barras ------------------------------------------------------------

ggplot(living_wage) +
  geom_bar(
    aes(x = reorder(nom_ent, salario_digno_neto*(rural == 1)), y = salario_digno_neto, fill = as.factor(rural)), 
    stat = "identity",
    position = "dodge"
    ) +
  theme_conasami() + 
  scale_fill_manual(
    values = c("0" = "#611232", "1" = "#98989A"),
    labels = c("Urbano", "Rural")
  ) +
  scale_color_manual(
    values = c("0" = "#611232", "1" = "#98989A"),
    labels = c("Urbano", "Rural")
  ) + 
  labs(title = "Salario Digno Neto por entidad federativa y ámbito",
       x = "",
       y = "", 
       fill= "", 
       caption = "") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          plot.title = element_text(hjust = 0.5)) 

ggsave(
  "graphs/bar_living_wage.png", 
  plot = last_plot(),
  dpi = 300
)

# base_mapa --------------------------------------------------------------------

mexico <- ne_states(country = "Mexico", returnclass = "sf")
mexico$name <- ifelse(mexico$name == "Distrito Federal", "Ciudad de México", mexico$name)

mexico <- mexico %>% 
  left_join(living_wage, by = c("name" = "nom_ent"))

# mapa salario digno -----------------------------------------------------------

salario_digno <- mexico %>% 
  mutate(valores = factor(case_when(
    salario_digno_neto >= 13000 & salario_digno_neto < 16000 ~ "$13,000 - $16,000", 
    salario_digno_neto >= 16000 & salario_digno_neto < 19000 ~ "$16,000 - $19,000", 
    salario_digno_neto >= 19000 & salario_digno_neto < 21000 ~ "$19,000 - $21,000", 
    salario_digno_neto >= 21000 & salario_digno_neto < 23000 ~ "$21,000 - $23,000", 
    salario_digno_neto >= 23000 ~ "$23,000 o más",
    is.na(name) ~ "$13,000 - $16,000"
  ), 
  levels = c(
    "$13,000 - $16,000",
    "$16,000 - $19,000",
    "$19,000 - $21,000",
    "$21,000 - $23,000",
    "$23,000 o más"
  )), 
  ambito = ifelse(is.na(name), "Urbano", ambito))

etiquetas <- c(
  "$13,000 - $16,000" = "#A7E3A0",  
  "$16,000 - $19,000"     = "#BFD39D",  
  "$19,000 - $21,000"   = "#D6A79F",  
  "$21,000 - $23,000"    = "#A84C5D",  
  "$23,000 o más"    = "#621132"   
)

sd_urbano <- ggplot(data = salario_digno %>% filter(ambito == "Urbano")) +
  geom_sf(aes(fill = valores), color = "black") +
  labs(title = "Urbano", fill = "") +
  scale_fill_manual(values = etiquetas) +
  theme_void() +
  theme(legend.position = c(0.2,0.2),
        text = element_text(family = "Noto Sans"),
        legend.key.size = unit(0.4, "cm"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

sd_urbano

ggsave("graphs/maps/mapa_wage_urbano.png", sd_urbano, dpi = 300)

sd_rural <- ggplot(data = salario_digno %>% filter(ambito == "Rural")) +
  geom_sf(aes(fill = valores), color = "black") +
  labs(title = "Rural", fill = "") +
  scale_fill_manual(values = etiquetas) +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(family = "Noto Sans"),
        legend.key.size = unit(0.4, "cm"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

sd_rural

ggsave("graphs/maps/mapa_wage_rural.png", sd_rural, dpi = 300)

combined <- sd_urbano +  
  sd_rural + 
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "") + 
  theme(plot.background = element_blank()) 

combined 

ggsave(
  "graphs/maps/mapa_wage_combined.png", 
  combined, 
  dpi = 300
)

rm(sd_urbano, sd_rural, salario_digno, etiquetas, combined)

# alimentos --------------------------------------------------------------------

alimentos <- mexico %>% 
  mutate(valores = factor(case_when(
    alimentos_individual >= 2000 & alimentos_individual < 2500 ~ "$2,000 - $2,500",
    alimentos_individual >= 2500 & alimentos_individual < 3000 ~ "$2,500 - $3,000",
    alimentos_individual >= 3000 & alimentos_individual < 3500 ~ "$3,000 - $3,500",
    alimentos_individual >= 3500 & alimentos_individual < 4000 ~ "$3,500 - $4,000",
    alimentos_individual >= 4000 ~ "$4,000 o más",
    is.na(name) ~ "$2,000 - $2,500"
    ), 
    levels = c(
      "$2,000 - $2,500",
      "$2,500 - $3,000",
      "$3,000 - $3,500",
      "$3,500 - $4,000",
      "$4,000 o más"
    )),
    ambito = ifelse(is.na(name), "Urbano", ambito))

etiquetas <- c(
  "$2,000 - $2,500" = "#A7E3A0",  
  "$2,500 - $3,000"     = "#BFD39D",  
  "$3,000 - $3,500"   = "#D6A79F",  
  "$3,500 - $4,000"    = "#A84C5D",  
  "$4,000 o más"    = "#621132"   
)

urbano <- ggplot(data = alimentos %>% filter(ambito == "Urbano")) +
  geom_sf(aes(fill = valores), color = "black") +
  labs(title = "Urbano", fill = "") +
  scale_fill_manual(values = etiquetas) +
  theme_void() +
  theme(legend.position = c(0.2,0.2),
        text = element_text(family = "Noto Sans"),
        legend.key.size = unit(0.4, "cm"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

urbano

ggsave("graphs/maps/mapa_alimentos_urbano.png", urbano, dpi = 300)

rural <- ggplot(data = alimentos %>% filter(ambito == "Rural")) +
  geom_sf(aes(fill = valores), color = "black") +
  labs(title = "Rural", fill = "") +
  scale_fill_manual(values = etiquetas) +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(family = "Noto Sans"),
        legend.key.size = unit(0.4, "cm"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

rural

ggsave("graphs/maps/mapa_alimentos_rural.png", rural, dpi = 300)

combined <- urbano + 
  rural + 
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "") + 
  theme(plot.background = element_blank()) 

combined

ggsave("graphs/maps/mapa_alimentos_combined.png", combined)

rm(urbano, rural, alimentos, etiquetas, combined)

# no alimento no vivienda ------------------------------------------------------

nanv <- mexico %>% 
  mutate(valores = factor(case_when(
    nanv >= 2800 & nanv < 4500 ~ "$2,800 - $4,500",
    nanv >= 4500 & nanv < 5000 ~ "$4,500 - $5,000",
    nanv >= 5000 & nanv < 5500 ~ "$5,000 - $5,500",
    nanv >= 5500 & nanv < 6000 ~ "$5,500 - $6,000",
    nanv >= 6000 ~ "$6,000 o más",
    is.na(name) ~ "$2,800 - $4,500"
  ), 
  levels = c(
    "$2,800 - $4,500",
    "$4,500 - $5,000",
    "$5,000 - $5,500",
    "$5,500 - $6,000",
    "$6,000 o más"
  )),
  ambito = ifelse(is.na(name), "Urbano", ambito))

etiquetas <- c(
  "$2,800 - $4,500" = "#A7E3A0",  
  "$4,500 - $5,000"     = "#BFD39D",  
  "$5,000 - $5,500"   = "#D6A79F",  
  "$5,500 - $6,000"    = "#A84C5D",  
  "$6,000 o más"    = "#621132"   
)

urbano <- ggplot(data = nanv %>% filter(ambito == "Urbano")) +
  geom_sf(aes(fill = valores), color = "black") +
  labs(title = "Urbano", fill = "") +
  scale_fill_manual(values = etiquetas) +
  theme_void() +
  theme(legend.position = c(0.2,0.2),
        text = element_text(family = "Noto Sans"),
        legend.key.size = unit(0.4, "cm"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

urbano

ggsave("graphs/maps/mapa_nanv_urbano.png", urbano, dpi = 300)

rural <- ggplot(data = nanv %>% filter(ambito == "Rural")) +
  geom_sf(aes(fill = valores), color = "black") +
  labs(title = "Rural", fill = "") +
  scale_fill_manual(values = etiquetas) +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(family = "Noto Sans"),
        legend.key.size = unit(0.4, "cm"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

rural

ggsave("graphs/maps/mapa_nanv_rural.png", rural, dpi = 300)

combined <- urbano + 
  rural + 
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "") + 
  theme(plot.background = element_blank()) 

combined

ggsave("graphs/maps/mapa_nanv_combined.png", combined)

rm(urbano, rural, nanv, etiquetas, combined)

# vivienda ---------------------------------------------------------------------

vivienda <- mexico %>% 
  mutate(valores = factor(case_when(
    vivienda_familiar >= 2000 & vivienda_familiar < 3500 ~ "$2,000 - $3,500",
    vivienda_familiar >= 3500 & vivienda_familiar < 4500 ~ "$3,500 - $4,500",
    vivienda_familiar >= 4500 & vivienda_familiar < 5500 ~ "$4,500 - $5,500",
    vivienda_familiar >= 5500 & vivienda_familiar < 6500 ~ "$5,500 - $6,500",
    vivienda_familiar >= 6500 ~ "$6,500 o más",
    is.na(name) ~ "$2,000 - $3,500"
  ), 
  levels = c(
    "$2,000 - $3,500",
    "$3,500 - $4,500",
    "$4,500 - $5,500",
    "$5,500 - $6,500",
    "$6,500 o más"
  )),
  ambito = ifelse(is.na(name), "Urbano", ambito))

etiquetas <- c(
  "$2,000 - $3,500" = "#A7E3A0",  
  "$3,500 - $4,500"     = "#BFD39D",  
  "$4,500 - $5,500"  = "#D6A79F",  
  "$5,500 - $6,500"   = "#A84C5D",  
  "$6,500 o más"    = "#621132"   
)

urbano <- ggplot(data = vivienda %>% filter(ambito == "Urbano")) +
  geom_sf(aes(fill = valores), color = "black") +
  labs(title = "Urbano", fill = "") +
  scale_fill_manual(values = etiquetas) +
  theme_void() +
  theme(legend.position = c(0.2,0.2),
        text = element_text(family = "Noto Sans"),
        legend.key.size = unit(0.4, "cm"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

urbano

ggsave("graphs/maps/mapa_vivienda_urbano.png", urbano, dpi = 300)

rural <- ggplot(data = vivienda %>% filter(ambito == "Rural")) +
  geom_sf(aes(fill = valores), color = "black") +
  labs(title = "Rural", fill = "") +
  scale_fill_manual(values = etiquetas) +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(family = "Noto Sans"),
        legend.key.size = unit(0.4, "cm"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

rural

ggsave("graphs/maps/mapa_vivienda_rural.png", rural, dpi = 300)

combined <- urbano + 
  rural + 
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "") + 
  theme(plot.background = element_blank()) 

combined

ggsave("graphs/maps/mapa_vivienda_combined.png", combined, dpi = 300)

rm(urbano, rural, vivienda, etiquetas, combined, living_wage, mexico)