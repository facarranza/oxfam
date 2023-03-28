library(googlesheets4)
library(dplyr)
library(tidyr)
gs4_deauth()
general_info <- read_sheet("https://docs.google.com/spreadsheets/d/1tjMuZuPliEdssJjqZtTKsOC8x5WR3ENwlWoCp-Dhhvk/edit#gid=0", "slug_descripcion")

slug_translate <- general_info |>
  select(slug, slug_es = nombre_es, slug_en = name, slug_pt = nome)
usethis::use_data(slug_translate, overwrite = TRUE)


slug_extras <-  general_info |>
  select(slug, slug_description_es = descripcion_es,
         slug_description_en = description,
         slug_description_pt = descrição,
         fuente, url)
usethis::use_data(slug_extras, overwrite = TRUE)


slug_filters <- general_info |>
  select(slug, filtro_fecha, filtro_pais, filtro_unidad)
usethis::use_data(slug_filters, overwrite = TRUE)


slug_viz <- read_sheet("https://docs.google.com/spreadsheets/d/1tjMuZuPliEdssJjqZtTKsOC8x5WR3ENwlWoCp-Dhhvk/edit#gid=0", "matriz_visualizaciones")
slug_agg <- slug_viz |>
  select(slug, agg = mapa) |>
  filter(agg %in% c("mean", "sum,mean", "sum"))
slug_agg$agg <- as.character(slug_agg$agg)
usethis::use_data(slug_agg, overwrite = TRUE)



# slug_viz_one <- read_sheet("https://docs.google.com/spreadsheets/d/1tjMuZuPliEdssJjqZtTKsOC8x5WR3ENwlWoCp-Dhhvk/edit#gid=0", "dashboard_1")
# slug_agg_one <- slug_viz_one |>
#   select(slug, agg = mapa) |>
#   filter(agg %in% c("mean", "sum,mean", "sum"))
# slug_agg_one$agg <- as.character(slug_agg_one$agg)
# usethis::use_data(slug_agg_one, overwrite = TRUE)


slug_viz <- slug_viz |>
  select(slug, map = mapa,
         bar = barras,
         line = linea,
         treemap,
         scatter = scatter_plot,
         sankey) |>
  gather("viz", "agg", -slug) |>
  filter(agg != "0") |> select(-agg)
usethis::use_data(slug_viz, overwrite = TRUE)


slug_comparisons <- read_sheet("https://docs.google.com/spreadsheets/d/1tjMuZuPliEdssJjqZtTKsOC8x5WR3ENwlWoCp-Dhhvk/edit#gid=0", "matriz_comparaciones")
slug_comparisons <- slug_comparisons |>
  select(indicador, slug = indicador_comparacion, notas)
usethis::use_data(slug_comparisons, overwrite = TRUE)

