webshot::install_phantomjs(force = F)
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
library(shinyjs)
library(urlshorteneR)

ui <- panelsPage(
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="custom.css"),
    tags$script(src="handler.js")
  ),
  useShi18ny(),
  useShinyjs(),
  busy_start_up(
    loader = tags$img(
      src = "icons/loading_gris.gif",
      width = 100
    ),
    mode = "manual",
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
          uiOutput("idiomas"),
          uiOutput("compartir"),
          uiOutput("downloads")
        ),
        color = "chardonnay",
        can_collapse = FALSE,
        body = div(
          verbatimTextOutput("aver"),
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
          img(src= 'icons/logos.svg',
              style = "border-top: 1px solid #252525;padding: 5px 0px;",
              align = "left", width = 300, height = 80))
  )
)


server <-  function(input, output, session) {



  # url params --------------------------------------------------------------

  url_params <- list(dash = NULL,
                     slug = NULL,
                     slug_comp = NULL,
                     pais = NULL,
                     und = NULL,
                     fech = NULL,
                     fech_form = NULL,
                     agg = NULL,
                     viz = NULL,
                     id_click = NULL,
                     cat_click = NULL)

  url_par <- reactive({
    shinyinvoer::url_params(url_params, session)
  })


  save_inputs <- reactive({
    req(parmesan_input())
    ls <- parmesan_input()
    #ls <- Filter(Negate(is.null), ls)
    ls
  })

  shared_link <- reactiveValues(facebook = NULL, twitter = NULL)

  observe({
    if (is.null(lang())) return()
    if (is.null(save_inputs())) return()
    save_inp <- save_inputs()
    slug = NULL
    slug_comp = NULL
    pais = NULL
    und = NULL
    fech = NULL
    fech_form = NULL
    agg = NULL
    viz = NULL
    id_click = NULL
    cat_click = NULL

    if (!is.null(save_inp$id_date_format)) fech_form <- paste0("fech_form=", paste0(save_inp$id_date_format, collapse = ","), "%26")
    if (!is.null(save_inp$id_slug_dates)) fech <- paste0("fech=", paste0(save_inp$id_slug_dates, collapse = ","), "%26")
    if (!is.null(save_inp$id_slug_countries)) pais <- paste0("pais=", paste0(save_inp$id_slug_countries, collapse = ","), "%26")
    if (!is.null(save_inp$id_slug_agg)) agg <- paste0("agg=", paste0(save_inp$id_slug_agg, collapse = ","), "%26")
    if (!is.null(save_inp$id_unidad)) und <- paste0("und=", paste0(save_inp$id_unidad, collapse = ","), "%26")
    if (!is.null(save_inp$id_slug_comparisons))  slug_comp <- paste0("slug_comp=", paste0(save_inp$id_slug_comparisons, collapse = ","), "%26")
    if (!is.null(save_inp$id_slug)) slug <- paste0("slug=", paste0(save_inp$id_slug, collapse = ","), "%26")
    if (!is.null(actual_but$active)) viz <- paste0("viz=",actual_but$active, "%26")
    if (!is.null(click_viz$id)) id_click <- paste0("id_click=", click_viz$id, "%26")
    if (!is.null(click_viz$cat)) cat_click <- paste0("cat_click=",click_viz$cat, "%26")

    shared_link$facebook <- stringi::stri_escape_unicode(paste0(viz, slug, slug_comp, pais, agg, und, fech, fech_form, id_click, cat_click, "lang=", lang()))
    shared_link$twitter <- paste0(viz, slug, slug_comp, pais, agg, und, fech, fech_form, id_click, cat_click, "lang=", lang())

  })



  # output$aver <- renderPrint({
  #   bitly_auth()
  #   bitly_create_bitlink(
  #     long_url = shared_link$twitter,
  #     domain = "bit.ly"
  #   )
  # })

  # Compartir ---------------------------------------------------------------

  output$compartir <- renderUI({
    #/*<img src="icons/vector.svg" width="8" height="8" id = "indicator">*/
    HTML(
      '
      <div class="dropdown-shared">
        <button class="dropbtn-shared">
        <img src="icons/compartir.svg" width="15" height="15">
        </button>
        <div class="dropdown-shared-content">
        <a class="needed" id="fc"><img src="icons/facebook.svg" width="15" height="15"></a>
        <a class="needed" id="tw"><img src="icons/twitter.svg" width="15" height="15"></a>
        <a class="needed" id="lk"><img src="icons/link.svg" width="15" height="15"></a>
        </div>
        </div>
      '
    )

  })

  shared_red <- reactiveValues(id = NULL)

  observe({
    if (is.null(input$last_click)) return()
    shared_red$id <- input$last_click
  })



  observeEvent(shared_red$id, {
    if (shared_red$id == "tw") {
      shinyjs::runjs(sprintf("window.open('%s')", paste0("https://twitter.com/intent/tweet?text=Hola&url=https://datasketch.shinyapps.io/oxfam_app/?", shared_link$twitter)))
    }
    if (shared_red$id == "fc") {
      shinyjs::runjs(sprintf("window.open('%s')", paste0("http://www.facebook.com/sharer.php?t=Hola&u=https://datasketch.shinyapps.io/oxfam_app/?", shared_link$facebook)))
    }
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


  output$idiomas <- renderUI({
    req(lang())

    available_lang <- setdiff(c("en", "es", "pt"), lang())
    available_lang <- paste0('<a href="?lang=', available_lang,
                             '" target="_self">',
                             stringr::str_to_title(available_lang), "</a>", collapse = "")


    HTML(
      paste0(
        '
         <div class="dropdown-shared">
        <button class="dropbtn-shared">',
        lang(),
        '</button>
        <div class="dropdown-shared-content">',
        available_lang,
        '</div>
        </div>
        '

      )
    )

  })



  # opciones de parmesan ----------------------------------------------------

  slug_opts <- reactive({
    req(lang())
    data_traductor <- slug_translate
    data_slug <- slug_translate
    if (!is.null(url_par()$inputs$dash)) {
      dash <- url_par()$inputs$dash
      if (dash == "one") data_slug <- slug_dash_one
      if (dash == "two") data_slug <- slug_dash_two
      if (dash == "three") data_slug <- slug_dash_three
      if (dash == "four") data_slug <- slug_dash_four
    }
    data_slug <- data.frame(slug = data_slug$slug) |> left_join(data_traductor)

    setNames(data_slug$slug, data_slug[[paste0("slug_", lang())]])
  })


  slug_select <- reactive({
    req(slug_opts())
    slug <- slug_translate$slug[1]
    if (!is.null(url_par()$inputs$slug)) slug <- url_par()$inputs$slug
    slug
  })



  possible_viz <- reactive({
    if (is.null(input$id_slug)) return()
    df <- slug_viz |> filter(slug %in% input$id_slug)
    c(df$viz, "table")
  })

  actual_but <- reactiveValues(active = NULL)

  observe({

    if (!is.null(url_par()$inputs$viz)) {
      actual_but$active <- url_par()$inputs$viz
    }

    req(possible_viz())
    if (is.null(input$viz_selection)) return()
    viz_rec <- possible_viz()
    if (input$viz_selection %in% viz_rec) {

      actual_but$active <- input$viz_selection
    } else {
      actual_but$active <- viz_rec[1]
    }

  })

  # observeEvent(input$viz_selection, {
  #
  # })
  #


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

  slug_comparisons_select <- reactive({
    if (is.null(url_par()$inputs$slug_comp)) return()
    url_par()$inputs$slug_comp
  })



  slug_comparation <- reactiveValues(id = NULL)

  observe({
    if (is.null(input$id_slug_comparisons)) return()
    slug_comparation$id <- input$id_slug_comparisons
  })

  observeEvent(input$id_slug, {
    slug_comparation$id <- NULL
  })

  observeEvent(input$viz_selection, {
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

  slug_agg_select <- reactive({
    req(slug_agg_opts())
    if (is.null(url_par()$inputs$agg)) return()
    url_par()$inputs$agg
  })

  data_filter_slug <- reactive({
    req(lang())
    req(slug_selected())
    d <- oxfam_data[[lang()]][[input$id_slug]]
    d
  })

  observe({
    if (is.null(data_filter_slug())) return()
    Sys.sleep(3)
    remove_start_up(timeout = 200)
  })

  slug_unidad_opts <- reactive({
    req(data_filter_slug())
    if (!"unidad" %in% names(data_filter_slug())) return()
    setNames(c("all", gsub("[[:space:]]", "_",unique(data_filter_slug()$unidad_id))),
             c(i_("all", lang()), unique(data_filter_slug()$unidad_id))
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

  slug_unidad_select <- reactive({
    if (is.null(show_unidad())) return()
    if (!show_unidad()) return()
    sel <- "all"
    if (!is.null(url_par()$inputs$und)) {
      sel <- strsplit(url_par()$inputs$und, ",") |> unlist()
    }
    sel
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
          f_u <- gsub("_", " ",input$id_unidad)
          d <- d |> filter(unidad_id %in% f_u)
        }
      }
    } else {
      ls <- oxfam_data[[lang()]][slug_selected()]
      # if (all(slug_selected() %in% c("vaccination_approvals_trials",
      #                                "school_closures", "stringency_index",
      #                                "product_pipeline"))) {
      #   d <- ls |> bind_rows()
      # } else {
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
      #}
    }
    #print(d)
    d
  })

  slug_countries_opts <- reactive({
    req(data_slug())
    req(lang())
    pais <- paste0("pais_", lang())
    setNames(c("all", sort(unique(data_slug()[[pais]]))),
             c(i_("all", lang()), sort(unique(data_slug()[[pais]]))))
  })


  slug_countries_sel <- reactive({
    sel <- "all"
    if (!is.null(url_par()$inputs$pais)) {
      sel <- strsplit(url_par()$inputs$pais, ",") |> unlist()
    }
    print(sel)
    sel
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

  min_date_sel <- reactive({
    req(min_date())
    sel <- min_date()
    if (!is.null(url_par()$inputs$fech)) {
      sel <- strsplit(url_par()$inputs$fech, ",") |> unlist()
      sel <- sel[1]
    }
    sel
  })

  max_date_sel <- reactive({
    req(max_date())
    sel <- max_date()
    if (!is.null(url_par()$inputs$fech)) {
      sel <- strsplit(url_par()$inputs$fech, ",") |> unlist()
      sel <- sel[length(sel)]
    }
    sel
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

  date_format_sel <- reactive({
    req(date_format_opts())
    if (is.null(url_par()$inputs$fech_form)) return()
    url_par()$inputs$fech_form
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
      df$fecha <- lubridate::ymd(df$fecha)
      df <- df |> arrange(fecha)
      #print(unique(df$fecha))
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
      paste_fnc <- function (x, collapse = "") {
        paste0(trimws(unique(x)), collapse = collapse)
      }
      if (length(unique(df$id)) != nrow(df)) {
        df1 <- df |> group_by(id) |>
          summarise(dplyr::across(dplyr::everything(), list(paste_fnc)))
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

    # if (slug_selected()[1] %in% c(#"product_pipeline",
    #   "doses_delivered_vaccine_donations", "immunization_campaigns",
    #   "covid_vaccine_agreements","geopolitics_vaccine_donations")) {
    #   print("PENDIENTE")
    # }

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
        var_other$cat <- c(paste0("pais_", lang()), "fecha")
        var_other$var_viz <- paste0("pais_", lang())
        var_other$num <- "valor"
        var_other$agg <- "mean"
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
    id_ct <- grep("fecha_ct", names(data))
    data <- data[,-id_ct]
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
                                           agg_extra = agg_extra,
                                           extra_sep = "<br/>")

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
        format_sample_num = "1,234.56",
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
      if (!unidad) unidad_label <- NULL#"{e}"
      opts$theme$palette_colors <- rev(c("#151E42", "#253E58", "#35606F", "#478388", "#5DA8A2", "#7BCDBE", "#A5F1DF"))
      # print("*************")
      # print(unidad)
      # if (unidad) {
      # opts$theme$tooltip_template <- "a {a}<br/>b {b}<br/>c {c}<br/>d {d}<br/> e {e} <br/> f {f}<br/>g {g}" #paste0("<b>{a}<br/> {b} ", unidad_label)
      # } else {
      #   opts$theme$tooltip_template <- "a {a}<br/>b {b}<br/>c {c}<br/>d {d}<br/>f {f}<br/>g {g}<br/> h {h}"
      # }
      opts$theme$tooltip_template <- paste0("<b>{a}<br/>",var_viz()$label_agg,": {b} <br/>", unidad_label)
      if (lang() != "en") {
        if (!unidad) unidad_label <- "{g}"
      }
      if (lang() == "es")  opts$theme$tooltip_template <- paste0("<b>{c}<br/>",var_viz()$label_agg,": {b} <br/>", unidad_label)

      if (lang() == "pt")  opts$theme$tooltip_template <- paste0("<b>{c}<br/>",var_viz()$label_agg,": {b} <br/>", unidad_label)

    } else {
      if (viz != "sankey") {
        pais <- paste0("{pais_", lang(), "}<br/>")
        valor <- paste0("{",var_viz()$label_agg, "}<br/>")
        unidad_label <- "{unidad}"
        if (!unidad) unidad_label <- paste0("{slug_", lang(), "}")
        fecha <- NULL
        if (viz %in% c("line", "scatter")) {
          opts$theme$axis_line_x_size <- 0
          if ("fecha" %in% names(data_viz())) fecha <- paste0("{fecha}<br/>")
        }
        tooltip <- paste0("<b>",pais, "</b>",
                          fecha, var_viz()$label_agg, ":",
                          valor, unidad_label)
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
    #print(data_viz())
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
    df <- data_load()
    #print(df)
    var_select <- c(c("id", paste0("slug_", lang()),
                      paste0("pais_", lang())), "fecha", "valor")

    if ("unidad" %in% names(df)) {
      var_select <- c(var_select, "unidad")
    }
    df <- df[,var_select]
    names_tr <- i_(names(df), lang = lang())
    names(df) <- names_tr
    df
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
      DT::dataTableOutput("dt_viz", height = height_viz, width = "100%")
    } else {
      withLoader(
        highchartOutput("viz_hgch", height = height_viz),
        type = "image", loader = "icons/loading_gris.gif")
    }

  })


  click_url <- reactiveValues(id = NULL, cat = NULL)
  observe({
    if (!is.null(url_par()$inputs$id_click)) {
      click_url$id <- url_par()$inputs$id_click
    }
    if (!is.null(url_par()$inputs$cat_click)) {
      click_url$cat <- url_par()$inputs$cat_click
    }
  })

  click_viz <- reactiveValues(id = NULL, cat = NULL)

  observe({
    print(click_url$id)
    if (!is.null(click_url$id)) {
      click_viz$id <- click_url$id
      print(click_viz$id)
      print(click_viz$cat)
    } else {
      if (is.null(input$hcClicked)) return()
      click_viz$id <- gsub("<br/>", " ", input$hcClicked$id)
      if ("cat" %in% names(input$hcClicked)) {
        click_viz$cat <- gsub("<br/>", " ", input$hcClicked$cat)
      }
    }

  })


  observeEvent(input$hcClicked, {
    click_url$id <- NULL
    click_url$cat <- NULL
  })

  observeEvent(input$id_slug, {
    click_viz$id <- NULL
    click_viz$cat <- NULL
  })

  observeEvent(input$viz_selection, {
    click_viz$id <- NULL
    click_viz$cat <- NULL
  })

  observeEvent(input$id_date_format, {
    click_viz$id <- NULL
    click_viz$cat <- NULL
  })


  viz_click <- reactive({
    req(viz_select())
    req(slug_selected())
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


    if (viz_select() == "line") {
      req(input$id_date_format)
      if (input$id_date_format == "anio_mes_dia") pais_click <- NULL
      if (slug_selected()[1] == "school_closures") {
        pais_click <- click_viz$id
        cat_click <- NULL
      }
    }

    if (length(slug_selected()) == 2) {
      pais_click <- NULL
      slug_click <- click_viz$id
    }

    if (viz_select() %in% c( "sankey")) {
      cat_click <- click_viz$id
      pais_click <- click_viz$cat
      fecha_click <- NULL
    }

    list(
      pais_click = pais_click,
      fecha_click = fecha_click,
      cat_click = cat_click,
      slug_click = slug_click
    )

  })

  data_click <- reactive({
    req(viz_click())
    req(data_load())
    if (is.null(click_viz$id)) return()
    df <- data_load()


    if (!is.null(viz_click()$pais_click)) {
      pais <- paste0("pais_", lang())
      if (viz_select() == "map") pais <- "pais_en"
      df <- df |> filter(!!dplyr::sym(pais) %in% viz_click()$pais_click)
    }


    if (!is.null(viz_click()$fecha_click)) {
      df$fecha <- as.character(df$fecha)
      fecha <- "fecha"
      if (viz_select() == "scatter") fecha <- "fecha_ct"
      df <- df |> filter(!!dplyr::sym(fecha) %in% viz_click()$fecha_click)
    }

    if (!is.null(viz_click()$cat_click)) {
      cat <- "unidad"
      df$unidad <- gsub("<br/>", " ", df$unidad)
      df <- df |> filter(!!dplyr::sym(cat) %in% viz_click()$cat_click)
    }

    df
  })

  slug_description <- reactive({
    req(viz_click())
    req(slug_selected())
    req(data_click())
    df <- data_click()
    slug <- slug_translate |> filter(slug %in% slug_selected())
    extra <- slug_extras |> filter(slug %in% slug_selected())
    extra <- extra |> left_join(slug, by = "slug")
    extra <- extra[, c(paste0("slug_", lang()), paste0("slug_description_", lang()), "fuente", "url")]

    if (!is.null(viz_click()$slug_click)) {
      extra <- extra |> filter(!!sym(paste0("slug_", lang())) %in% viz_click()$slug_click)
    }
    extra
  })

  viz_extra_click <- reactive({
    req(viz_select())
    req(viz_theme())

    req(data_click())
    df <- data_click()
    #print(df)
    if (nrow(df) == 0) return()
    if (is.null(input$id_slug_agg)) return()
    if (!"fecha" %in% names(df)) return()
    if (length(slug_selected()) == 1) {
      if (viz_select() == "scatter") return()
    }
    viz_line <- TRUE


    if (!viz_select() %in% c("line", "scatter")) {
      if (length(unique(df$fecha)) == 1) return()
      df$fecha <- format(lubridate::ymd(df$fecha), "%Y-%m")
    }

    if (viz_select() == "line") {
      req(input$id_date_format)
      df$fecha <- df$fecha_all
      if (input$id_date_format %in% c("anio", "anio_mes")) {
        df$fecha <- format(df$fecha, "%Y-%m")
        if (length(unique(df$fecha)) == 1) {
          df$fecha <- df$fecha_all
        }
        if (length(unique(df$fecha)) == 1) return()
      }
      if (input$id_date_format == "anio_mes_dia") {
        viz_line <- FALSE
        df$fecha <- df[[paste0("pais_", lang())]]
      }
    }




    var_num <- "valor"
    label_agg <- i_(input$id_slug_agg, lang())
    if ("valor" %in% names(df)) var_num <- "valor"
    theme <- viz_theme()
    theme$theme$tooltip_template <- paste0("{fecha} <br/> ",
                                           label_agg, ": {", label_agg, "}" )
    theme$theme$shiny_clickable <- FALSE

    if (length(slug_selected()) == 2) {
      slug_num <- slug_translate |> filter(slug %in% slug_selected())
      var_num <- slug_num[[paste0("slug_", lang())]]
      label_agg <- var_num
      if (viz_select() == "scatter") {
        viz_line <- FALSE
        df$fecha <- df[[paste0("pais_", lang())]]
        df <- df |> tidyr::spread(paste0("slug_", lang()), "valor") #|> tidyr::drop_na()
        df[[var_num[1]]][is.na(df[[var_num[1]]])] <- 0
        df[[var_num[2]]][is.na(df[[var_num[2]]])] <- 0
      }
    }

    agg <- input$id_slug_agg
    if (length(slug_selected()) == 1) {
      if (slug_selected()[1] == "school_closures") {
        agg <- "count"
      }}

    # print(df)

    df_dates <-  dsdataprep::aggregation_data(data = df,
                                              agg = agg,
                                              agg_name = label_agg,
                                              group_var = "fecha",
                                              to_agg = var_num,
                                              extra_col = FALSE)

    var_cat <- "fecha"
    var_num <- label_agg
    if (length(slug_selected()) == 2) {
      theme$theme$tooltip_template <- NULL
    }

    if (viz_line) {
      viz <- hgch_line(df_dates, var_dat = var_cat, var_num =  var_num, opts = theme)
    } else {
      viz <- hgch_bar(df_dates, var_cat = var_cat, var_num =  var_num, opts = theme)
    }
    viz

  })

  output$viz_extra <- renderHighchart({
    req(viz_extra_click())
    viz_extra_click()
  })

  output$show_viz_click <- renderUI({
    req(data_click())
    req(slug_selected())
    df <- data_click()
    options(scipen=999)
    if (!is.null(viz_extra_click())) {
      v <- highchartOutput("viz_extra")
    } else {
      v <- NULL
      if (viz_select() == "scatter") {
        if (length(slug_selected()) == 1) {
          v <- HTML(paste0("<ul><li>",
                           format(data_click()$valor, big.mark = ",", small_mark = "."),
                           "</li></ul>"))
        }
      } else {
        if (nrow(data_click()) == 1) {
          v <- HTML(paste0(i_("valor", lang()), ": ",
                           format(data_click()$valor, big.mark = ",", small_mark = "."),
                           "<br/>",
                           data_click()$unidad))
        } else {
          v <- HTML(paste0("<br/><b>", i_("valor", lang()), ":</b> ",
                           format(data_click()$valor, big.mark = ",", small_mark = "."),
                           "<br/>",
                           data_click()$unidad))
        }
      }

    }

    v
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
    if (is.null(slug_description())) return(tx)
    if (nrow(slug_description()) == 0) return(tx)
    req(data_click())
    df <- data_click()
    click <- viz_click()
    text_description <- NULL

    df_desc <- slug_description()
    text_description <- div (class = "head-click",
                             div(class = "title-click", df_desc[[paste0("slug_", lang())]]),
                             div(class = "description-click", df_desc[[paste0("slug_description_", lang())]])
    )

    fuentes <- div(class = "fuente-click",
                   HTML(
                     paste0(i_("fuente", lang()),": ",
                            "<a href=", df_desc$url, " target='_blank'>", df_desc$fuente,"</a>")
                   )
    )

    pais <- NULL
    cat <- NULL
    fecha <- NULL
    if (!is.null(viz_click()$pais_click)) {
      pais <- paste0("<b>",unique(df[[paste0("pais_", lang())]]), "</b><br/>")
    }
    if (!is.null(viz_click()$fecha_click)) {
      cat <- paste0("<b>",unique(df$fecha), "</b><br/>")
    }
    if (!is.null(viz_click()$cat_click)) {
      fecha <- paste0("<b>",viz_click()$cat_click, "</b><br/>")
    }


    click <- div(class = "click-select",
                 HTML(
                   paste0(pais, cat, fecha))
    )

    tx <- div (
      div (
        text_description,
        click,
        uiOutput("show_viz_click"),
        fuentes
      )
    )
    tx
  })

  # output$test_url <- renderPrint({
  #   list(
  #  viz_click(),
  #  data_click()
  #   )
  # #input$hcClicked
  #  # text_click()
  # })


  output$downloads <- renderUI({
    if (is.null(actual_but$active)) return()
    if (actual_but$active != "table") {
      dsmodules::downloadImageUI("download_viz",
                                 dropdownLabel = icon("download"),#i_("download", lang()),
                                 formats = c("jpeg", "pdf", "png", "html"),
                                 display = "dropdown", dropdownWidth = 60,
                                 text = ""#i_("download", lang())
      )
    } else {
      dsmodules::downloadTableUI("dropdown_table",
                                 dropdownLabel = icon("download"),
                                 formats = c("csv", "xlsx", "json"),
                                 display = "dropdown",
                                 text = ""#i_("download", lang())
      )
    }
  })

  observe({
    dsmodules::downloadTableServer("dropdown_table",
                                   element = reactive(data_down()),
                                   formats = c("csv", "xlsx", "json"))
    dsmodules::downloadImageServer("download_viz",
                                   element = reactive(hgch_viz()),
                                   lib = "highcharter",
                                   formats = c("jpeg", "pdf", "png", "html"),
                                   file_prefix = "plot")
  })

}

shinyApp(ui, server)
