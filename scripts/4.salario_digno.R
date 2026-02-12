#_______________________________________________________________________________

# Objetivo: Hacer suma y graficas de los componentes de la canasta digna

# Autor: Coordinación de Análisis de la Economía Laboral

#_______________________________________________________________________________

rm(list = ls()); gc()

pacman:::p_load(tidyverse, dplyr, data.table)

source("scripts/theme_conasami.R")

#_______________________________________________________________________________

alimentos <- fread("finaldata/alimentos/7.canasta_alimentaria.csv")

nanv <- fread("finaldata/nanv/1.canasta_nanv.csv") %>%
  mutate(ambito = ifelse(rural == 1, "Rural", "Urbano")) %>%
  select(nom_ent, ambito, NANV)

vivienda <- fread("finaldata/vivienda/canasta_vivienda.csv") %>%
  mutate(
    vivienda_mensual = pred / 3,
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
    ),
    ambito = ifelse(rural == 1, "Rural", "Urbano")
  )

canasta <- alimentos %>%
  left_join(nanv, by = c("nom_ent", "ambito")) %>%
  left_join(vivienda, by = c("nom_ent", "ambito")) %>%
  select(
    nom_ent,
    cve_ent,
    ambito,
    rural,
    canasta_alimentos,
    vivienda_mensual,
    NANV
         ) %>%
  mutate(
    alimentos_familiar = canasta_alimentos * 4,
    nanv_familiar = NANV * 4,
    ingreso_digno_familiar = (alimentos_familiar + vivienda_mensual + nanv_familiar)*1.05,
    salario_digno = ingreso_digno_familiar / 2,
  ) %>% 
  arrange(
    nom_ent,
    ambito 
  )

fwrite(canasta, "salario_digno.csv", bom = TRUE)

proporciones <- canasta %>%
  select(
    nom_ent,
    ambito,
    alimentos_familiar,
    vivienda_mensual,
    nanv_familiar,
    ingreso_digno_familiar
  ) %>%
  pivot_longer(
    cols = c(alimentos_familiar, vivienda_mensual, nanv_familiar),
    names_to = "componente",
    values_to = "monto"
  ) %>% 
  group_by(nom_ent, ambito) %>%
  mutate(
    proporcion = monto / sum(monto) * 100
  ) %>% 
  ungroup() %>% 
  select(nom_ent, ambito, componente, proporcion)

ggplot(proporciones %>% filter(nom_ent == "Nacional")) +
  #circulo
  geom_bar(aes(x = 2, y = proporcion, fill = componente),
           stat = "identity", color = "white") + 
  facet_wrap(~ambito) +
  coord_polar(theta = "y", start = 0) +
  xlim(0.5, 2.5) +
  theme_conasami() +
  scale_fill_manual(
    values = c(
      "alimentos_familiar" = "#a57f2c",
      "vivienda_mensual" = "#98989A",
      "nanv_familiar" = "#4C4C4C"
    ),
    labels = c(
      "alimentos_familiar" = "Alimentos (familiar)",
      "vivienda_mensual" = "Vivienda (mensual)",
      "nanv_familiar" = "NANV (familiar)"
    )
  ) 

















