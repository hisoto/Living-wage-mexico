#_______________________________________________________________________________
# 1a_nombres_canasta.R
#
# Coordinación de Análisis de la Economía Laboral (CAEL) — Conasami
# Salario Digno — etiquetado de la Canasta Alimentaria
#
# Propósito: post-procesar las cantidades medianas de la canasta alimentaria
#   (output del script 1) para etiquetar cada clave de producto con su nombre
#   legible (concepto) y su unidad de medida, y consolidar un cuadro reproducible
#   que alimenta el anexo "Productos en la Canasta Alimentaria" del documento
#   extendido.
#
# Inputs:  finaldata/alimentos/5.cantidades_ca_urbano.csv
#          finaldata/alimentos/5.cantidades_ca_rural.csv
#
# Output (finaldata/alimentos/):
#   12.productos_canasta.csv   ambito, clave, nombre, cantidad, unidad
#
# Notas: las cantidades son las medianas ponderadas del estrato de referencia
#   estimadas por el script 1; este script solo agrega etiquetas. La unidad no
#   existe en la ENIGH (se toma del Manual de la Canasta Digna); la Leche entera
#   fresca (011411) se estandariza a litros en ambos ámbitos. Migrado desde
#   scripts/otros/alimentos/nombres_canastas.R.
#_______________________________________________________________________________

rm(list = ls()); gc()
options(scipen = 999)

if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  tidyverse,
  data.table
)

# ── catálogo de nombres (clave -> concepto) ──────────────────────────────────
# Unión de las claves de la canasta urbana y rural.

nombre_producto <- function(clave) {
  case_when(
    clave == "011111" ~ "Arroz en grano",
    clave == "011131" ~ "Tortillas de maíz",
    clave == "011132" ~ "Tortillas de harina de trigo",
    clave == "011133" ~ "Pan",
    clave == "011134" ~ "Pan de caja",
    clave == "011135" ~ "Galletas dulces",
    clave == "011137" ~ "Pan dulce a granel",
    clave == "011150" ~ "Spaghetti, pastas italianas y pastas para sopa",
    clave == "011192" ~ "Tostadas",
    clave == "011221" ~ "Bistec de res",
    clave == "011224" ~ "Molida de res",
    clave == "011228" ~ "Costillas y chuleta de puerco",
    clave == "01122F" ~ "Pollo entero o en piezas (excepto pierna, muslo y pechuga)",
    clave == "011231" ~ "Jamón de puerco",
    clave == "011252" ~ "Chorizo de puerco",
    clave == "011322" ~ "Atún enlatado",
    clave == "011411" ~ "Leche entera fresca",
    clave == "011433" ~ "Crema",
    clave == "011451" ~ "Queso fresco",
    clave == "011454" ~ "Queso Oaxaca o asadero",
    clave == "011460" ~ "Yogurt y bebidas fermentadas a base de leche",
    clave == "011481" ~ "Huevo de gallina",
    clave == "011512" ~ "Otros aceites vegetales comestibles",
    clave == "011592" ~ "Chicharrón",
    clave == "011611" ~ "Aguacate",
    clave == "011616" ~ "Otros plátanos: Chiapas, dominico, guineo, manzano, dorado, portalimón y Roatán",
    clave == "011621" ~ "Limón",
    clave == "011633" ~ "Manzana y perón",
    clave == "011712" ~ "Lechuga",
    clave == "011721" ~ "Calabaza",
    clave == "011722" ~ "Chayote",
    clave == "011723" ~ "Chile jalapeño",
    clave == "011725" ~ "Chile serrano",
    clave == "011727" ~ "Jitomate (tomate rojo)",
    clave == "011728" ~ "Pepino",
    clave == "011729" ~ "Tomate verde",
    clave == "011743" ~ "Cebolla",
    clave == "011746" ~ "Nopal",
    clave == "011748" ~ "Zanahoria",
    clave == "011751" ~ "Papa",
    clave == "011761" ~ "Frijol en grano",
    clave == "011796" ~ "Papas fritas en bolsa o a granel",
    clave == "011797" ~ "Frijol procesado",
    clave == "011810" ~ "Azúcar",
    clave == "011911" ~ "Alimentos preparados para consumo inmediato",
    clave == "011942" ~ "Cilantro",
    clave == "012100" ~ "Jugos de frutas o verduras envasados",
    clave == "012201" ~ "Café soluble",
    clave == "012501" ~ "Agua natural embotellada",
    clave == "012601" ~ "Refrescos sabor cola",
    clave == "012602" ~ "Refrescos de otros sabores",
    clave == "111111" ~ "Desayunos",
    clave == "111112" ~ "Comidas",
    clave == "111113" ~ "Cenas",
    clave == "111124" ~ "Otros alimentos cocinados",
    clave == "111126" ~ "Pollos rostizados",
    clave == "181124" ~ "Pierna, muslo o pechuga de pollo con hueso",
    clave == "181125" ~ "Pierna, muslo o pechuga de pollo sin hueso",
    clave == "181126" ~ "Chorizo de pollo, jamón de pollo, pavo y otras aves; nuggets de pollo y otras aves, salchicha, mortadela, tocino de pollo, pavo y otras aves, etcétera",
    clave == "181134" ~ "Plátano verde y Tabasco",
    TRUE ~ NA_character_
  )
}

# ── catálogo de unidades (clave -> unidad) ───────────────────────────────────
# Tomado del Manual de la Canasta Digna. Default = kg; la Leche entera fresca
# (011411) se estandariza a litros en ambos ámbitos.

unidad_producto <- function(clave) {
  case_when(
    clave %in% c("011411", "011460", "011512", "012100",
                 "012501", "012601", "012602")                ~ "lt",
    clave %in% c("011911", "111111", "111112", "111113",
                 "111124", "111126")                          ~ "pieza",
    TRUE                                                       ~ "kg"
  )
}

# ── consolidación ────────────────────────────────────────────────────────────

etiquetar <- function(archivo, etiqueta) {
  fread(archivo) |>
    transmute(
      ambito   = etiqueta,
      clave,
      nombre   = nombre_producto(clave),
      cantidad,
      unidad   = unidad_producto(clave)
    )
}

productos <- bind_rows(
  etiquetar("finaldata/alimentos/5.cantidades_ca_urbano.csv", "Urbano"),
  etiquetar("finaldata/alimentos/5.cantidades_ca_rural.csv",  "Rural")
) |>
  arrange(ambito, clave)

# verificación: ninguna clave debe quedar sin nombre
faltantes <- productos |> filter(is.na(nombre)) |> pull(clave)
if (length(faltantes) > 0) {
  warning("Claves sin nombre en el catálogo: ", paste(faltantes, collapse = ", "))
}

fwrite(productos, "finaldata/alimentos/12.productos_canasta.csv", bom = TRUE)

message("1a_nombres_canasta.R: listo. Output en finaldata/alimentos/12.productos_canasta.csv")
