library(shiny)
library(shinybusy)
library(shinyinvoer)
library(shinypanels)
library(shinycustomloader)
library(shi18ny)
library(parmesan)
library(oxfam)
library(hgchmaps)
library(highcharter)
library(hgchmagic)
library(dsmodules)
library(dsapptools)
library(dplyr)

ui <- panelsPage(
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="custom.css"),
    tags$script(src="handler.js")
  ),
  useShi18ny(),
  busy_start_up(
    loader = tags$img(
      src = "icons/loading_gris.gif",
      width = 100
    ),
    mode = "auto",
    color = "#435b69",
    background = "#FFF"
  ),
  langSelectorInput("lang", position = "fixed"),
  panel(title = ui_("data_filter"),
        id = "controls-style",
        collapse = FALSE,
        can_collapse = FALSE,
        width = 300,
        body = div(
          uiOutput("controls")
        )
  ),
  panel(title = ui_("data_viz"),
        id = "viz-style",
        header_right = div(
          class = "head-viz",
          div(class = "viz-style",
              uiOutput("viz_icons")),
          uiOutput("downloads")
        ),
        color = "chardonnay",
        can_collapse = FALSE,
        body = div(
          verbatimTextOutput("test_url"),
          uiOutput("viz_out")
          #verbatimTextOutput("debug")
        )
  ),
  panel(title = ui_("data_dtail"),
        id = "detail-style",
        collapse = FALSE,
        can_collapse = FALSE,
        width = 300,
        body = div(
          uiOutput("click_info")
        ),
        footer = tags$a(
          href="https://www.datasketch.co", target="blank",
          img(src= 'icons/logos.svg', style = "border-top: 1px solid #252525;",
              align = "left", width = 300, height = 80))
  )
)


server <-  function(input, output, session) {



  # url params --------------------------------------------------------------

  url_params <- list(slug = NULL, pais = NULL, viz = NULL)

  url_par <- reactive({
    shinyinvoer::url_params(url_params, session)
  })



  # Idiomas -----------------------------------------------------------------

  i18n <- list(
    defaultLang = "en",
    availableLangs = c("es","en", "pt")
  )
  lang <- callModule(langSelector,"lang", i18n = i18n, showSelector=FALSE)


  observeEvent(lang(),{
    shinyjs::delay(500, uiLangUpdate(input$shi18ny_ui_classes, lang()))
  })

  slug_opts <- reactive({
    req(lang())
    setNames(slug_translate$slug, slug_translate[[paste0("slug_", lang())]])
  })


  possible_viz <- reactive({
    if (is.null(input$id_slug)) return()
    df <- slug_viz |> filter(slug %in% input$id_slug)
    c(df$viz, "table")
  })

  actual_but <- reactiveValues(active = NULL)

  observe({
    req(possible_viz())
    if (is.null(input$viz_selection)) return()
    viz_rec <- possible_viz()
    if (input$viz_selection %in% viz_rec) {
      actual_but$active <- input$viz_selection
    } else {
      actual_but$active <- viz_rec[1]
    }
  })

  output$viz_icons <- renderUI({
    req(lang())
    req(possible_viz())
    viz <- possible_viz()
    viz_label <- i_(possible_viz(), lang())

    suppressWarnings(
      shinyinvoer::buttonImageInput("viz_selection",
                                    " ",
                                    images = viz,
                                    tooltips = viz_label,
                                    path = "img/",
                                    active = actual_but$active,
                                    imageStyle = list(shadow = FALSE,
                                                      borderSize = "1px",
                                                      borderColor = "#252525",
                                                      padding = "4px")
      )
    )
  })

  viz_select <- reactive({
    req(actual_but$active)
    actual_but$active
  })



  slug_comparisons_opts <- reactive({
    if (is.null(input$id_slug)) return()
    first_ind <- input$id_slug
    table_ind <- slug_comparisons |>
      filter(indicador %in% first_ind)
    if (nrow(table_ind) == 0) return()
    table_ind <- table_ind |>
      left_join(slug_translate, by = c("slug" = "slug"))
    setNames(table_ind$slug, table_ind[[paste0("slug_", lang())]])
  })


  slug_selected <- reactive({
    req(viz_select())
    if (viz_select() %in% c("line", "scatter")) {
      c(input$id_slug, input$id_slug_comparisons)
    } else {
      input$id_slug
    }
  })


  slug_agg_filter <- reactive({
    req(input$id_slug)
    df <- slug_agg |> filter(slug %in% input$id_slug)
    df
  })

  slug_agg_show <- reactive({
    req(slug_agg_filter())
    df <- slug_agg_filter()
    show <- TRUE
    if (nrow(df) == 0) show <- FALSE
    show
  })

  slug_agg_opts <- reactive({
    req(input$id_slug)
    req(slug_agg_filter())
    df <- slug_agg_filter()
    if (nrow(df) == 0) return()
    agg_opts <- strsplit(df$agg, ",") |> unlist()
    setNames(agg_opts, i_(agg_opts, lang()))
  })


  # var_select <- reactive({
  #   req(viz_select())
  #   req(slug_selected())
  #   viz <- viz_select()
  #   var_cat <- c("id", paste0("pais_", lang()), paste0("slug_", lang()))
  #
  #   var_num <- "value"
  #   if (length(slug_selected()) > 1) var_num <- slug_selected()
  #
  # })


  # special cases
  # vaccination_approvals_trials
  # school_closures
  # stringency_index
  # product_pipeline

  data_filter_slug <- reactive({
    req(lang())
    req(slug_selected())
    d <- oxfam_data[[lang()]][[input$id_slug]]
    d
  })

  slug_unidad_opts <- reactive({
    req(data_filter_slug())
    if (!"unidad" %in% names(data_filter_slug())) return()
    unique(data_filter_slug()$unidad)
  })

  show_unidad <- reactive({
    req(slug_selected())
    show <- FALSE

    if (!is.null(slug_unidad_opts())) {
      if (length(slug_selected()) == 1) show <- TRUE
      if (length(slug_unidad_opts()) == 1) show <- FALSE
    }
    show
  })


  data_slug <- reactive({
    req(data_filter_slug())
    req(slug_selected())
    if (length(slug_selected()) == 1) {
      d <- data_filter_slug()
      if (show_unidad()) {
        req(input$id_unidad)
        d <- d |> filter(unidad %in% input$id_unidad)
      }
    } else {
      ls <- oxfam_data[[lang()]][slug_selected()]
      if (all(slug_selected() %in% c("vaccination_approvals_trials",
                                     "school_closures", "stringency_index",
                                     "product_pipeline"))) {
        d <- ls |> bind_rows()
      } else {
        id_valor <- grep("valor", names(ls[[1]]))
        names(ls[[1]])[id_valor] <- unique(ls[[1]][[paste0("slug_", lang())]])
        id_valor <- grep("valor", names(ls[[2]]))
        names(ls[[2]])[id_valor] <- unique(ls[[2]][[paste0("slug_", lang())]])
        #d <- ls
        d <- ls |> purrr::reduce(inner_join, by = c("fecha", "pais_en", "pais_es", "pais_pt"))
      }
    }

  })

  slug_countries_opts <- reactive({
    req(data_slug())
    req(lang())
    pais <- paste0("pais_", lang())
    setNames(c("all", sort(unique(data_slug()[[pais]]))),
             c(i_("all", lang()), sort(unique(data_slug()[[pais]]))))
  })

  observe({
    if (is.null(input$id_slug_countries)) return()

    id_p <- which(input$id_slug_countries %in% "all")
    if (identical(id_p, integer())) {
      return()
    } else {
      if (length(input$id_slug_countries) == 1) return()
      if (length(input$id_slug_countries) > 1) {
        #print(id_p)
        if (id_p == 1) {
          sc <- setdiff(input$id_slug_countries, "all")
          updateSelectizeInput(session,
                               inputId = "id_slug_countries",
                               selected = sc)
        } else {
          updateSelectizeInput(session,
                               inputId = "id_slug_countries",
                               selected = "all")
        }
      }
    }

  })

  data_countries <- reactive({
    req(data_slug())
    req(input$id_slug_countries)
    df <- data_slug()
    pais <- paste0("pais_", lang())
    if (all(input$id_slug_countries %in% "all")) {
      df <- df
    } else {
      df <- df |>
        filter(!!dplyr::sym(pais) %in% input$id_slug_countries)
    }
    df
  })


  have_date <- reactive({
    req(data_countries())
    show <- "fecha" %in% names(data_countries())
    if (show) {
      show <- min(data_countries()$fecha, na.rm = TRUE) != max(data_countries()$fecha, na.rm = TRUE)
    }
    show
  })

  date_start <- reactive({
    req(lang())
    i_("date_start", lang())
  })

  date_end <- reactive({
    req(lang())
    req(have_date())
    if (!have_date()) return()
    i_("date_end", lang())
  })

  min_date <- reactive({
    req(data_countries())
    req(have_date())
    if (!have_date()) return()
    df <- data_countries()
    min(df$fecha, na.rm = TRUE)
  })

  max_date <- reactive({
    req(data_countries())
    req(have_date())
    if (!have_date()) return()
    df <- data_countries()
    max(df$fecha, na.rm = TRUE)
  })



  date_format_opts <- reactive({
    req(have_date())
    if (!have_date()) return()
    setNames(c("anio", "anio_mes", "anio_mes_dia"),
             i_(c("anio", "anio_mes", "anio_mes_dia"), lang())
    )
  })

  # Renderizar inputs con parmesan ------------------------------------------


  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_lang <- reactive({
    i_(parmesan, lang(), keys = c("label", "choices", "text"))
  })
  output_parmesan("controls", parmesan = parmesan_lang,
                  input = input, output = output, session = session,
                  env = environment())



  data_load <- reactive({
    if (is.null(have_date())) return()
    req(data_countries())
    df <- data_countries()
    if (have_date()) {
      if (is.null(input$id_slug_dates)) return()
      range <- input$id_slug_dates
      df <- dsapptools:::filter_ranges(df, range, "fecha")
    }
    df
  })






  output$test_url <- renderPrint({
    data_load()
  })

}

shinyApp(ui, server)
