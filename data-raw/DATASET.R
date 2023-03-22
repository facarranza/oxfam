## code to prepare `DATASET` dataset goes here
library(DBI)
library(dplyr)
library(purrr)
library(tidyr)
library(googlesheets4)
library(oxfam)

gs4_deauth()
readRenviron(".Renviron")

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = "oxfam-project.c2oxbyqo9drv.us-east-1.rds.amazonaws.com",
  port = 5432,
  dbname = "oxfam",
  user = "postgres",
  password = Sys.getenv("DB_PASSWORD")
)
tables <- dbListTables(con)

indicadores <- dplyr::tbl(con, "indicadores") |> dplyr::collect()

available_slug <- unique(indicadores$slug)
unit_info <-  read_sheet("https://docs.google.com/spreadsheets/d/1tjMuZuPliEdssJjqZtTKsOC8x5WR3ENwlWoCp-Dhhvk/edit#gid=0", "filtros_detalle_unidades")
unit_translate <- unit_info |>
  select(slug, unidad, unidad_es, unidad_en, unidade_pt) |>
  drop_na(unidad)
unit_translate$unidade_pt <- coalesce(unit_translate$unidade_pt, unit_translate$unidad)

countries_info <- read_sheet("https://docs.google.com/spreadsheets/d/1tjMuZuPliEdssJjqZtTKsOC8x5WR3ENwlWoCp-Dhhvk/edit#gid=0", "variable_pais")
countries_translate <- countries_info |>
  select(pais = pais_es, pais_en, pais_pt)

translate_func <- function(df, slug_i) {
  df <- df |>
    filter(slug %in% slug_i) |>
    separate_rows(unidad, sep = "\\|")
  df <- df |> left_join(unit_translate)
  df <- df |> left_join(countries_translate)
  df <- df |> left_join(slug_translate)
  df$unidad_es <- coalesce(df$unidad_es, df$unidad)
  df$unidad_es[df$unidad_es == ""] <- NA
  df$unidad_en <- coalesce(df$unidad_en, df$unidad)
  df$unidad_en[df$unidad_en == ""] <- NA
  df$fecha_ct <- as.numeric(as.POSIXct(df[["fecha"]], format="%Y-%m-%d"))*1000
  df
}


slug_spanish <- map(available_slug, function(slug_i) {
  df <- translate_func(indicadores, slug_i)
  df_es <- df |> select(id, slug, slug_es, fecha,
                        pais_es = pais, pais_en, pais_pt,
                        valor, unidad = unidad_es, fecha_ct)
  df_es <- Filter(function(x) !all(is.na(x)), df_es)
  df_es
})
names(slug_spanish) <- available_slug
slug_spanish

slug_english <- map(available_slug, function(slug_i) {
  df <- translate_func(indicadores, slug_i)
  df_en <- df |> select(id, slug, slug_en, fecha,
                        pais_es = pais, pais_en, pais_pt,
                        valor, unidad = unidad_en, fecha_ct)
  df_en <- Filter(function(x) !all(is.na(x)), df_en)
  df_en
})
names(slug_english) <- available_slug
slug_english


slug_portuges <- map(available_slug, function(slug_i) {
  df <- translate_func(indicadores, slug_i)
  df_pt <- df |> select(id, slug, slug_pt, fecha,
                        pais_es = pais, pais_en, pais_pt,
                        valor, unidad = unidade_pt, fecha_ct)
  df_pt <- Filter(function(x) !all(is.na(x)), df_pt)
  df_pt
})
names(slug_portuges) <- available_slug
slug_portuges


# all data
oxfam_data <- list(
  es = slug_spanish,
  en = slug_english,
  pt = slug_portuges
)

usethis::use_data(oxfam_data, overwrite = TRUE)

dash_6 <- read_sheet("https://docs.google.com/spreadsheets/d/1tjMuZuPliEdssJjqZtTKsOC8x5WR3ENwlWoCp-Dhhvk/edit#gid=0", "dashboard_6")
dash_6$visualizacion <- gsub("\\s*(\\([^()]*(?:(?1)[^()]*)*\\))", "", dash_6$visualizacion, perl=TRUE)
dash_6 <- dash_6 |> separate_rows(indicador, sep = ",")
ind_6 <- unique(dash_6$indicador)
slug_spanish_6 <- map(ind_6, function (i) {
  slug_spanish[[i]]
})
names(slug_spanish_6) <- ind_6

slug_english_6 <- map(ind_6, function (i) {
  slug_english[[i]]
})
names(slug_english_6) <- ind_6

slug_portuges_6 <- map(ind_6, function (i) {
  slug_portuges[[i]]
})
names(slug_portuges_6) <- ind_6

oxfam_6 <- list(
  es = slug_spanish_6,
  en = slug_english_6,
  pt = slug_portuges_6
)

usethis::use_data(oxfam_6, overwrite = TRUE)

questions_dash_6 <- dash_6 |> select(pregunta_es = pregunta, subpergunta_es = subpregunta,
                                     pregunta_en = question, subpregunta_en = subquestion,
                                     pregunta_pt = pergunta, subpregunta_pt = subpergunta,
                                     indicador, viz = visualizacion)

usethis::use_data(questions_dash_6, overwrite = TRUE)
#https://jsfiddle.net/gh/get/library/pure/highcharts/highcharts/tree/master/samples/highcharts/demo/combo-multi-axes
