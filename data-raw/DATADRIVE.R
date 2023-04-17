library(googlesheets4)
library(dplyr)
library(tidyr)
gs4_deauth()

url_book <- "https://docs.google.com/spreadsheets/d/1tjMuZuPliEdssJjqZtTKsOC8x5WR3ENwlWoCp-Dhhvk/edit#gid=0"
general_info <- read_sheet(url_book, "slug_descripcion")

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


slug_viz <- read_sheet(url_book, "matriz_visualizaciones")
slug_viz$mapa <- as.character(slug_viz$mapa)
slug_viz$barras <- as.character(slug_viz$barras)
slug_viz$mapa[slug_viz$mapa == 0] <- NA
slug_viz$mapa <- coalesce(slug_viz$mapa, slug_viz$barras)
slug_viz$mapa <- gsub("pais,|valor|count()|\\(|\\)", "", slug_viz$mapa)
slug_agg <- slug_viz |>
  select(slug, agg = mapa) |>
  filter(agg %in% c("mean", "sum,mean", "sum"))
slug_agg$agg <- as.character(slug_agg$agg)
usethis::use_data(slug_agg, overwrite = TRUE)


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


slug_comparisons <- read_sheet(url_book, "matriz_comparaciones")
slug_comparisons <- slug_comparisons |>
  select(indicador, slug = indicador_comparacion, notas)
usethis::use_data(slug_comparisons, overwrite = TRUE)


slug_viz_one <- read_sheet(url_book, "dashboard_1")
slug_dash_one <- slug_viz_one |>
  select(slug)
usethis::use_data(slug_dash_one, overwrite = TRUE)

slug_viz_two <- read_sheet(url_book, "dashboard_2")
slug_dash_two <- slug_viz_two |>
  select(slug)
usethis::use_data(slug_dash_two, overwrite = TRUE)


slug_viz_three <- read_sheet(url_book, "dashboard_3")
slug_dash_three <- slug_viz_three |>
  select(slug)
usethis::use_data(slug_dash_three, overwrite = TRUE)


slug_viz_four <- read_sheet(url_book, "dashboard_4")
slug_dash_four <- slug_viz_four |>
  select(slug)
usethis::use_data(slug_dash_four, overwrite = TRUE)

