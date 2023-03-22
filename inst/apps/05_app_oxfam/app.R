library(shiny)
library(shinybusy)
library(shinyinvoer)
library(shinypanels)
library(shinycustomloader)
library(shi18ny)
library(parmesan)
library(oxfam)
library(hgchmaps)
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
          uiOutput("viz_show")#,

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


  slug_comparation <- reactiveValues(id = NULL)

  observe({
    if (is.null(input$id_slug_comparisons)) return()
    slug_comparation$id <- input$id_slug_comparisons
  })

  observeEvent(input$id_slug, {
    slug_comparation$id <- NULL
  })


  slug_selected <- reactive({
    req(viz_select())
    if (viz_select() %in% c("line", "scatter")) {
      c(input$id_slug, slug_comparation$id)
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


  data_filter_slug <- reactive({
    req(lang())
    req(slug_selected())
    d <- oxfam_data[[lang()]][[input$id_slug]]
    d
  })

  slug_unidad_opts <- reactive({
    req(data_filter_slug())
    if (!"unidad" %in% names(data_filter_slug())) return()
    setNames(c("all", unique(data_filter_slug()$unidad)),
             c(i_("all", lang()), unique(data_filter_slug()$unidad))
    )
  })

  show_unidad <- reactive({
    req(data_filter_slug())
    show <- FALSE
    if ("unidad" %in% names(data_filter_slug())) {
      show <- length(unique(data_filter_slug()$unidad)) > 1
    }
    show
  })


  observe({
    if (is.null(show_unidad())) return()
    if (is.null(input$id_unidad)) return()

    id_u <- which(input$id_unidad %in% "all")
    if (identical(id_u, integer())) {
      return()
    } else {
      if (length(input$id_unidad) == 1) return()
      if (length(input$id_unidad) > 1) {
        if (id_u == 1) {
          sc <- setdiff(input$id_unidad, "all")
          updateSelectizeInput(session,
                               inputId = "id_unidad",
                               selected = sc)
        } else {
          updateSelectizeInput(session,
                               inputId = "id_unidad",
                               selected = "all")
        }
      }
    }

  })


  data_slug <- reactive({
    req(data_filter_slug())
    req(slug_selected())
    req(viz_select())

    if (length(slug_selected()) == 1) {
      if (is.null(show_unidad())) return()
      d <- data_filter_slug()
      if (show_unidad()) {
        req(input$id_unidad)
        if (!"all" %in% input$id_unidad) {
          d <- d |> filter(unidad %in% input$id_unidad)
        }
      }
    } else {
      ls <- oxfam_data[[lang()]][slug_selected()]
      if (all(slug_selected() %in% c("vaccination_approvals_trials",
                                     "school_closures", "stringency_index",
                                     "product_pipeline"))) {
        d <- ls |> bind_rows()
      } else {
        if (viz_select() == "line") {
          id_valor <- grep("valor", names(ls[[1]]))
          names(ls[[1]])[id_valor] <- unique(ls[[1]][[paste0("slug_", lang())]])
          id_valor <- grep("valor", names(ls[[2]]))
          names(ls[[2]])[id_valor] <- unique(ls[[2]][[paste0("slug_", lang())]])
          d <- ls |> purrr::reduce(inner_join,
                                   by = c("fecha", "pais_en", "pais_es", "pais_pt"),
                                   multiple = "any")
        } else {
          d <- ls |> bind_rows() |> tidyr::drop_na(valor)
        }
      }
    }
    d
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
    have_date <- FALSE

    if ("fecha" %in% names(data_countries())) {
      have_date  <- length(unique(data_countries()$fecha)) > 1
    }
    have_date
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
    req(viz_select())
    if (viz_select() != "line") return()
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
    req(data_countries())
    req(viz_select())
    if (is.null(have_date())) return()
    agg <- input$id_slug_agg
    if (is.null(slug_agg_show())) agg <- NULL
    df <- data_countries()
    if (have_date()) {
      if (is.null(input$id_slug_dates)) return()
      range <- input$id_slug_dates
      df <- dsapptools:::filter_ranges(df, range, "fecha")

      if (viz_select() == "line") {

        if (is.null(input$id_date_format)) return()
        df$fecha_all <- df$fecha
        if (input$id_date_format == "anio_mes") {
          df$fecha <- format(df$fecha, "%Y-%m")
        }
        if (input$id_date_format == "anio") {
          df$fecha <- format(df$fecha, "%Y")
        }
      }
    }

    if ("valor" %in% names(df)) {
      if (length(unique(df$id)) != nrow(df)) {
        df1 <- df |> group_by(id) |>
          summarise(dplyr::across(dplyr::everything(), list(dsapptools:::paste_vector)))
        names(df1) <- names(df)
        df <- df1
        df$valor <- as.numeric(df$valor)
      }
    }

    df
  })


  var_viz <- reactive({
    req(viz_select())
    req(slug_selected())
    if (is.null(have_date())) return()

    viz <- viz_select()
    var_date <- NULL
    if (viz %in% c("line", "scatter")) {
      if (have_date()) var_date <- "fecha"
    }
    var_cat <- paste0("pais_", lang())
    var_num <- "valor"
    var_cat_viz <- var_cat
    if (viz == "map") var_cat_viz <- "pais_en"
    agg <- input$id_slug_agg
    if (!is.null(agg)) {
      label_agg <- i_(agg, lang())
    }
    var <- list(
      agg = agg,
      cat = c(var_cat, var_date),
      num = var_num,
      label_agg = label_agg,
      var_viz = var_cat_viz,
      var_viz_date = var_date
    )

    if (slug_selected()[1] %in% c(#"product_pipeline",
      "doses_delivered_vaccine_donations", "immunization_campaigns",
      "covid_vaccine_agreements","geopolitics_vaccine_donations")) {
      print("PENDIENTE")
    }

    if (slug_selected()[1] %in% "product_pipeline") {
      if (viz != "map") {
        var_pipe <- list(
          var_viz = c(var_cat_viz, "unidad"),
          cat = c(var_cat_viz, "unidad"),
          num = NULL,
          label_agg = "Total",
          agg = "count"
        )
        var <- modifyList(var, var_pipe)
      }
    }

    if (slug_selected()[1] %in% c("school_closures")) {
      var_other <- list(
        var_viz = c("unidad", var_cat_viz),
        cat = c("unidad", var_cat_viz),
        num = NULL,
        label_agg = "Total",
        agg = "count")
      if (viz == "line") {
        var_other$cat <- c("unidad", "fecha")
        var_other$var_viz <- "unidad"
      }
      var <- modifyList(var, var_other)
    }

    if (length(slug_selected()) == 2) {
      if (viz == "line") {
        req(data_load())
        data <- data_load()
        var_double <- list(
          var_viz = NULL,
          cat = "fecha",
          num = setdiff(c(unique(data[[paste0("slug_", lang(), ".x")]]),
                          unique(data[[paste0("slug_", lang(), ".y")]])), NA)
        )
        var_double$label_agg <- var_double$num
        var <- modifyList(var, var_double)
      } else {
        var_double <- list(
          var_viz = paste0("slug_", lang()),
          cat = c("slug", "fecha"),
          num = "valor"
        )
        var_double$label_agg <- var_double$num
        var <- modifyList(var, var_double)
      }
    }


    if (viz == "sankey") {
      var_sankey <- list(
        var_viz = c("unidad", var_cat_viz),
        cat = c("unidad", var_cat_viz),
        num = "valor"
      )
      var <- modifyList(var, var_sankey)
    }

    var
  })

  data_viz <- reactive({
    req(data_load())
    req(var_viz())
    req(viz_select())
    req(slug_selected())

    data <- data_load()

    var_cat <- var_viz()$cat
    if (is.null(var_cat)) var_cat <- var_viz()$var_viz_date
    var_num <- var_viz()$num
    label_agg <- var_viz()$label_agg
    agg <- var_viz()$agg
    agg_extra <- agg
    if (agg == "count") agg_extra <- "sum"

    if (!is.null(var_viz()$agg)) {
      data <- dsdataprep::aggregation_data(data = data,
                                           agg = agg,
                                           agg_name = label_agg,
                                           group_var = var_cat,
                                           to_agg = var_num,
                                           extra_col = TRUE,
                                           agg_extra = agg_extra)

      var <- c(unique(var_viz()$var_viz, var_viz()$var_viz_date), label_agg)
      if (length(var_num) == 2) {
        data <- data |> select({{ var }})
        data$..labels <- " "
      } else {
        data <- data |> select({{ var }}, everything())
      }
    }

    data
  })


  viz_func <- reactive({
    req(viz_select())
    if (viz_select() == "table") return()
    viz <- NULL
    if (viz_select() == "map") {
      viz <- "hgch_choropleth_GnmNum"
    } else {
      viz <- paste0("hgch_", viz_select())
    }

    viz

  })


  viz_theme <- reactive({
    req(viz_select())
    viz <- viz_select()
    opts <- list(
      theme = list(
        marker_radius = 0,
        text_family = "Barlow",
        legend_family = "Barlow",
        text_color = "#0F1116",
        background_color = "#FFFFFF",
        grid_x_width = 0,
        label_wrap = 70,
        axis_line_y_size = 1,
        axis_line_x_size = 1,
        axis_line_color = "#CECECE",
        shiny_cursor = "pointer",
        shiny_clickable = TRUE,
        palette_colors = c("#47BAA6", "#151E42", "#FF4824", "#FFCF06", "#FBCFA4", "#FF3D95", "#B13168")
      )
    )

    req(data_viz())
    unidad <- FALSE
    if ("unidad" %in% names(data_viz())) unidad <- TRUE

    if (viz == "map") {
      unidad_label <- "{i}"
      if (!unidad) unidad_label <- "{e}"
      opts$theme$palette_colors <- rev(c("#151E42", "#253E58", "#35606F", "#478388", "#5DA8A2", "#7BCDBE", "#A5F1DF"))
      # print("*************")
      # print(unidad)
      # if (unidad) {
      # opts$theme$tooltip_template <- "a {a}<br/>b {b}<br/>c {c}<br/>d {d}<br/> e {e} <br/> f {f}<br/>g {g}" #paste0("<b>{a}<br/> {b} ", unidad_label)
      # } else {
      #   opts$theme$tooltip_template <- "a {a}<br/>b {b}<br/>c {c}<br/>d {d}<br/>f {f}<br/>g {g}<br/> h {h}"
      # }

      opts$theme$tooltip_template <- paste0("<b>{a}<br/> {b} ", unidad_label)
      if (lang() != "en") {
        if (!unidad) unidad_label <- "{g}"
      }
      if (lang() == "es")  opts$theme$tooltip_template <- paste0("<b>{c}<br/> {b} ", unidad_label)

      if (lang() == "pt")  opts$theme$tooltip_template <- paste0("<b>{c}<br/> {b} ", unidad_label)

    } else {
      if (viz != "sankey") {
        pais <- paste0("{pais_", lang(), "}")
        valor <- paste0("{",var_viz()$label_agg, "}")
        unidad_label <- "{unidad}"
        if (!unidad) unidad_label <- paste0("{slug_", lang(), "}")
        fecha <- NULL
        if (viz %in% c("line", "scatter")) {
          opts$theme$axis_line_x_size <- 0
          if ("fecha" %in% names(data_viz())) fecha <- paste0("{fecha}<br/>")
        }
        tooltip <- paste0("<b>",pais, "</b><br/>",
                          fecha,
                          valor, " ", unidad_label)
        opts$theme$tooltip_template <- tooltip
      }
    }

    opts

  })

  hgch_viz <- reactive({
    req(viz_func())
    req(data_viz())
    req(var_viz())
    req(viz_theme())

    var_num <- var_viz()$label_agg
    var_date <- var_viz()$var_viz_date
    var_cat <- var_viz()$var_viz
    if (length(var_num) == 2) {
      var_date <- 'fecha'
      var_cat <- NULL
    }

    do.call(viz_func(), list(
      data = data_viz(),
      var_cat = var_cat,
      var_dat = var_date,
      var_num = var_num,
      map_name = "latamcaribbean_countries",
      opts = viz_theme()
    ))
  })

  output$viz_hgch <- renderHighchart({
    req(hgch_viz())
    hgch_viz()
  })


  data_down <- reactive({
    req(data_load())
    data_load()
  })

  output$dt_viz <- DT::renderDataTable({
    req(viz_select())
    if (viz_select() != "table") return()
    req(data_down())
    df <- data_down()
    df <- dplyr::as_tibble(data_down())
    dtable <- DT::datatable(df,
                            rownames = F,
                            selection = 'none',
                            escape = FALSE,
                            options = list(
                              scrollX = T,
                              fixedColumns = TRUE,
                              fixedHeader = TRUE,
                              autoWidth = TRUE,
                              scrollY = "500px")
    )
    dtable
  })

  output$viz_show <- renderUI({
    req(viz_select())
    height_viz <- 650
    if(!is.null(input$dimension)) height_viz <- input$dimension[2] - 150

    if (viz_select() == "table") {
      DT::dataTableOutput("dt_viz", height = height_viz, width = input$dimension[1] - 400)
    } else {
      withLoader(
        highchartOutput("viz_hgch", height = height_viz),
        type = "image", loader = "icons/loading_gris.gif")
    }

  })


  click_viz <- reactiveValues(id = NULL, cat = NULL)

  observe({
    if (is.null(input$hcClicked)) return()
    click_viz$id <- gsub("<br/>", " ", input$hcClicked$id)
    if ("cat" %in% names(input$hcClicked)) {
      click_viz$cat <- gsub("<br/>", " ", input$hcClicked$cat)
    }
  })

  observeEvent(input$id_slug, {
    click_viz$id <- NULL
    click_viz$cat <- NULL
  })

  observeEvent(input$viz_selection, {
    click_viz$id <- NULL
    click_viz$cat <- NULL
  })



  data_click <- reactive({
    req(viz_select())
    req(slug_selected())
    req(data_load())
    if (is.null(click_viz$id)) return()
    df <- data_load()
    pais_click <- click_viz$id
    fecha_click <- click_viz$cat
    cat_click <- NULL
    slug_click <- NULL

    if (viz_select() %in% c("bar")) {
      if (!is.null(click_viz$cat)) {
        if (slug_selected()[1] != "product_pipeline") {
          pais_click <- click_viz$cat
          cat_click <- click_viz$id
        } else {
          pais_click <- click_viz$id
          cat_click <- click_viz$cat
        }
        fecha_click <- NULL
      }
    }

    if (viz_select() %in% c( "treemap")) {
      if (!is.null(click_viz$cat)) {
        if (slug_selected()[1] != "product_pipeline") {
          cat_click <- click_viz$cat
        } else {
          pais_click <- click_viz$cat
          cat_click <- click_viz$id
        }
        fecha_click <- NULL
      }
    }

    if (length(slug_selected()) == 2) {
      pais_click <- NULL
      slug_click <- click_viz$id
    }


    if (!is.null(pais_click)) {
      pais <- paste0("pais_", lang())
      df <- df |> filter(!!dplyr::sym(pais) %in% pais_click)
    }

    if (!is.null(fecha_click)) {
      fecha <- "fecha"
      if (viz_select() == "scatter") {
        print(df)
        fecha <- "fecha_ct"
      }
      df <- df |> filter(!!dplyr::sym(fecha) %in% fecha_click)
    }

    df
    # list(
    #   pais = pais_click,
    #   fecha = fecha_click,
    #   cat = cat_click,
    #   slug = slug_click
    # )
  })


  output$click_info <- renderUI({
    req(lang())
    if (lang() == "en") {
      tx <- HTML("<div class = 'click'>
               <img src='click/click.svg' class = 'click-img'/><br/>
               <b>Click on the visualisation</b> to see more information.")
    }
    if (lang() == "es") {
      tx <- HTML("<div class = 'click'>
               <img src='click/click.svg' class = 'click-img'/><br/>
               Da <b>clic sobre la visualización</b> para ver más información.")
    }
    if (lang() == "pt") {
      tx <- HTML("<div class = 'click'>
               <img src='click/click.svg' class = 'click-img'/><br/>
               <b>Clique na visualização</b> para ver mais informações.")
    }
    if (is.null(click_viz$id)) return(tx)
    tx <- click_viz$id
    tx
  })

  output$test_url <- renderPrint({
    # list(
    #   click_viz$id,
    #   click_viz$cat
    # )
    data_click()
  })


  output$downloads <- renderUI({
    if (is.null(actual_but$active)) return()
    if (actual_but$active != "table") {
      dsmodules::downloadImageUI("download_viz",
                                 dropdownLabel = i_("download", lang()),
                                 formats = c("jpeg", "pdf", "png", "html"),
                                 display = "dropdown",
                                 text = i_("download", lang()))
    } else {
      dsmodules::downloadTableUI("dropdown_table",
                                 dropdownLabel = i_("download", lang()),
                                 formats = c("csv", "xlsx", "json"),
                                 display = "dropdown", text = i_("download", lang()))
    }
  })

  observe({
    dsmodules::downloadTableServer("dropdown_table",
                                   element = data_down(),
                                   formats = c("csv", "xlsx", "json"))
    dsmodules::downloadImageServer("download_viz",
                                   element = hgch_viz(),
                                   lib = "highcharter",
                                   formats = c("jpeg", "pdf", "png", "html"),
                                   file_prefix = "plot")
  })

}

shinyApp(ui, server)
