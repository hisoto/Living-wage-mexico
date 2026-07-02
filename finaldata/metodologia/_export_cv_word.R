# ── exportar tabla de CV a CSV/Excel para pegar en Word ───
# Reproduce @tbl-representatividad-cv del documento extendido en formato ancho:
# Nacional arriba, entidades alfabéticas; columnas Urbano/Rural x 4 rubros, en %.
rm(list = ls()); gc()
options(scipen = 999)

pacman::p_load(dplyr, tidyr, readr, openxlsx)

base <- "finaldata/metodologia"
rep_cv <- read_csv(file.path(base, "representatividad_cv.csv"), show_col_types = FALSE)

pct <- function(x) ifelse(is.na(x), "", paste0(format(round(x * 100, 1), nsmall = 1), "%"))

rubros_ord <- c("Alimentos", "Vivienda", "NANV", "Gasto corriente")

cv_wide <- rep_cv |>
  filter(ambito %in% c("Urbano", "Rural")) |>
  mutate(rubro = factor(rubro, levels = rubros_ord)) |>
  select(nom_ent, ambito, rubro, cv) |>
  pivot_wider(names_from = c(ambito, rubro), values_from = cv,
              names_sep = "__", names_sort = TRUE)

cv_wide <- bind_rows(
  cv_wide |> filter(nom_ent == "Nacional"),
  cv_wide |> filter(nom_ent != "Nacional") |> arrange(nom_ent)
)

ord_cols <- paste(rep(c("Urbano", "Rural"), each = length(rubros_ord)),
                  rubros_ord, sep = "__")

tab <- cv_wide |>
  select(nom_ent, all_of(ord_cols)) |>
  mutate(across(-nom_ent, pct))

col_nm <- c("Entidad", rep(rubros_ord, 2))

# ── CSV (encabezado simple, una fila) ───
tab_csv <- tab
names(tab_csv) <- c("Entidad",
                    paste0("Urbano_", rubros_ord),
                    paste0("Rural_", rubros_ord))
write_excel_csv(tab_csv, file.path(base, "representatividad_cv_word.csv"))

# ── Excel (encabezado de dos niveles Urbano/Rural, listo para pegar) ───
wb <- createWorkbook()
addWorksheet(wb, "CV")

# fila 1: grupos; fila 2: rubros; datos desde fila 3
writeData(wb, "CV", x = t(c("Entidad", "Urbano", rep("", 3), "Rural", rep("", 3))),
          startRow = 1, colNames = FALSE)
writeData(wb, "CV", x = t(col_nm), startRow = 2, colNames = FALSE)
writeData(wb, "CV", x = tab, startRow = 3, colNames = FALSE)

mergeCells(wb, "CV", cols = 1, rows = 1:2)          # Entidad
mergeCells(wb, "CV", cols = 2:5, rows = 1)          # Urbano
mergeCells(wb, "CV", cols = 6:9, rows = 1)          # Rural

hdr <- createStyle(textDecoration = "bold", halign = "center",
                   valign = "center", border = "TopBottomLeftRight",
                   fgFill = "#611232", fontColour = "white")
addStyle(wb, "CV", hdr, rows = 1:2, cols = 1:9, gridExpand = TRUE)

body <- createStyle(halign = "right", border = "TopBottomLeftRight")
addStyle(wb, "CV", body, rows = 3:(nrow(tab) + 2), cols = 2:9, gridExpand = TRUE)
ent  <- createStyle(halign = "left", border = "TopBottomLeftRight")
addStyle(wb, "CV", ent, rows = 3:(nrow(tab) + 2), cols = 1, gridExpand = TRUE)
nac  <- createStyle(textDecoration = "bold", halign = "left",
                    border = "TopBottomLeftRight")
addStyle(wb, "CV", nac, rows = 3, cols = 1, gridExpand = TRUE)

setColWidths(wb, "CV", cols = 1, widths = 22)
setColWidths(wb, "CV", cols = 2:9, widths = 11)
freezePane(wb, "CV", firstActiveRow = 3, firstActiveCol = 2)

saveWorkbook(wb, file.path(base, "representatividad_cv_word.xlsx"), overwrite = TRUE)

cat("Listo:\n - ", file.path(base, "representatividad_cv_word.csv"), "\n - ",
    file.path(base, "representatividad_cv_word.xlsx"), "\n",
    "Filas:", nrow(tab), "\n")
