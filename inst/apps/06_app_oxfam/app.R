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
library(webshot2)
library(reactable)
webshot::install_phantomjs()
#TODO falta hacer zoom a América en el mapa, recibir como parametro URL el rango de fechas, ordenar y ajustar color de boton download

ui <- panelsPage(
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="custom.css"),
    tags$script(src="handler.js")
  ),
  useShi18ny(),
  busy_start_up(
    loader = tags$img(
      src = "loading_gris.gif",
      width = 100
    ),
    mode = "auto",
    color = "#435b69",
    background = "#FFF"
  ),
  langSelectorInput("lang", position = "fixed"),
  panel(title = ui_("question"),
        id = "controls-style",
        collapse = FALSE,
        can_collapse = FALSE,
        width = 300,
        body = div(
          #div(id = "myDiv", style = "font-family: 'IBM Plex Sans'; font-weight: 500; font-size: 14px; line-height: 18.2px;,  background-color:#252525;" ),
          uiOutput("button_questions")
        ),

        footer = tags$a(
          href="https://www.datasketch.co", target="blank",
          img(src= 'logos_es.svg',
              style = "border-top: 1px solid #252525;padding: 5px 0px;",
              align = "left", width = 300, height = 80))
  ),
  panel(title = ui_("subpregunta"),
        id = "controls-style2",

        can_collapse = FALSE,
        width = 300,
        body = div(

          #shinycustomloader::withLoader(
          uiOutput("button_subquestions")
          # type = "html", loader = "loader4"
          #)

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

          verbatimTextOutput("debug"),

          #  shinycustomloader::withLoader(
          uiOutput("country"),
          uiOutput("viz_view")
          #   type = "html", loader = "loader4"
          #   )

        )
  )

)



server <-  function(input, output, session) {

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


  # url params --------------------------------------------------------------

  url_params <- list(question = NULL, subquestion = NULL, viz = NULL)

  url_par <- reactive({
    shinyinvoer::url_params(url_params, session)
  })


  # Preguntas y subpreguntas ------------------------------------------------


  data_questions <- reactive({
    req(lang())
    dash_ques <- questions_dash_6
    dash_ques[, c("ind_pregunta", "ind_subpregunta",
                  paste0("pregunta_", lang()),
                  paste0("subpregunta_", lang()),
                  "viz", "indicador")]
  })

  output$button_questions <- renderUI({
    req(data_questions())
    df_b <- unique(data_questions()$ind_pregunta)[1]

    buttons <- dsapptools:::make_buttons(ids = unique(data_questions()$ind_pregunta),
                                         labels = unique(data_questions()[[paste0("pregunta_", lang())]]),
                                         default_active = df_b
    )
    buttons
  })


  ques_sel <- reactive({
    qs <- input$last_click
    if (is.null(qs)) qs <- "q_4"
    qs
  })

  data_subquestions <- reactive({
    req(data_questions())
    req(ques_sel())
    df <- data_questions()
    df <- df |> filter(ind_pregunta %in% ques_sel())
    df
  })

  output$button_subquestions <- renderUI({
    req(data_subquestions())
    df_b <- unique(data_subquestions()$ind_subpregunta)[1]
    buttons <- dsapptools:::make_buttons(ids = unique(data_subquestions()$ind_subpregunta),
                                         labels = unique(data_subquestions()[[paste0("subpregunta_", lang())]]),
                                         default_active = df_b,
                                         class="needed_sub",
                                         class_active = "basic_active_sub")
    buttons
  })


  subques_sel <- reactiveValues(id = NULL)

  observe({
    sq <- input$last_click_sub
    subques_sel$id <- sq
    if (is.null(sq)) subques_sel$id <- "q_4_31"
  })

  observeEvent(input$last_click, {
    req(data_subquestions())
    subques_sel$id <- unique(data_subquestions()$ind_subpregunta)[1]
  })


  questions_select <- reactive({
    req(subques_sel$id)
    req(data_subquestions())
    data_subquestions() |> filter(ind_subpregunta %in% subques_sel$id)
  })



  # Visualizaciones disponibles ---------------------------------------------



  possible_viz <- reactive({
    if (is.null(questions_select())) return()
    if (nrow(questions_select()) == 0) return()
    viz <- unique(
      strsplit(questions_select()$viz, split = ",") |>
        unlist()
    )
    viz <- gsub("treeemap", "treemap", viz)
    c(viz, "table")
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


  data_slug <- reactive({
    tryCatch({
      req(questions_select())
      req(viz_select())
      slug <- unique(questions_select()$indicador)
      if (length(slug) == 1) {
        d <- oxfam_6[[lang()]][[slug]]
      } else {
        ls <- oxfam_6[[lang()]][slug]
        if (viz_select() %in% c("line", "bar")) {
          id_valor <- grep("valor", names(ls[[1]]))
          names(ls[[1]])[id_valor] <- unique(ls[[1]][[paste0("slug_", lang())]])
          id_valor <- grep("valor", names(ls[[2]]))
          names(ls[[2]])[id_valor] <- unique(ls[[2]][[paste0("slug_", lang())]])
          d <- ls |> purrr::reduce(inner_join,
                                   by = c("fecha", "pais_en", "pais_es", "pais_pt"),
                                   multiple = "any")
        } else {
          d <- ls |> bind_rows()
          if ("valor" %in% names(d)) {
            d <- d |> tidyr::drop_na(valor)
          }
        }
      }
      d
    },
    error = function(cond) {
      return()
    })
  })


  output$country <- renderUI({
    req(data_slug())
    req(viz_select())
    slug <- unique(questions_select()$indicador)

    if(viz_select() %in% c("bar","line") & "school_closures" %in% slug) {
      sel_country <- unique(data_slug()[[paste0("pais_", lang())]])
      shiny::selectizeInput("country",
                            label= i_("pais",lang()),
                            choices=  sel_country,
                            selected= sel_country[1],
                            multiple =TRUE,
                            options = list(
                              placeholder = "All",
                              plugins = list("remove_button","drag_drop"))
      )
    }

  })



  # Filtro y duplicados -----------------------------------------------------


  data_filter <- reactive({
    req(data_slug())
    req(viz_select())
    viz <- viz_select()
    df <- req(data_slug())
    slug <- unique(questions_select()$indicador)
    paste_fnc <- function (x, collapse = "") {
      paste0(trimws(unique(x)), collapse = collapse)
    }

    if(viz_select() %in% c("bar","line") & "school_closures" %in% slug) {
      if (!is.null(input$country)) {
        pais <- paste0("pais_", lang())
        df <- df |> filter(!!sym(pais) %in% input$country)
    s  }
    }

    if ("id" %in% names(df)) {
      if (length(unique(df$id)) != nrow(df)) {
        df1 <- df |> group_by(id) |>
          summarise(dplyr::across(dplyr::everything(), list(paste_fnc)))
        names(df1) <- names(df)
        df <- df1
        df$valor <- as.numeric(df$valor)
      }
    }

    if (length(slug) == 2) {
      if (viz == "bar") {
        df$fecha <- format(df$fecha, "%Y-%m")
      }
    }



    df
  })




  # traductor de slugs ------------------------------------------------------

  slug_trans <- reactive({
    req(questions_select())
    slug_id <- unique(questions_select()$indicador)
    slug_df <- slug_translate |> filter(slug %in% slug_id)
    slug_df[[paste0("slug_", lang())]]
  })


  # Variables a visualizar --------------------------------------------------

  var_viz <- reactive({
    req(viz_select())
    viz <- viz_select()
    if (viz == "table") return()
    req(slug_trans())
    req(data_filter())
    df <- data_filter()

    slug <- unique(questions_select()$indicador)
    pais <- paste0("pais_", lang())
    if (viz == "map") pais <- "pais_en"
    var_viz <- c(pais, "fecha", "valor")
    type_viz <- "CatDatNum"
    num_viz  <- 3

    if (length(slug) == 1) {
      if (length(unique(df$fecha)) == 1 |
          viz %in% c("map", "bar", "treemap", "sankey")) {
        var_viz <- setdiff(var_viz, "fecha")
        type_viz <- "CatNum"
        num_viz  <- 2
      }
      # if (length(unique(df$unidad)) > 1) {
      #   var_viz <- gsub("valor", "unidad", var_viz)
      # }
    } else {
      if (viz != "scatter") {
        type_viz <- "DatNumNum"
        num_viz  <- 3
        if (viz == "bar"){
          type_viz <- "CatNumNum"
          num_viz  <- 3
        }
        var_viz <- c("fecha", slug_trans())
      }
    }

    list(
      var_viz = var_viz,
      type_viz = type_viz,
      num_viz  = num_viz
    )

  })



  # Tipo de viz -------------------------------------------------------------


  viz_func <- reactive({
    req(viz_select())
    req(var_viz())

    if (viz_select() == "table") return()
    viz <- NULL
    if (viz_select() == "map") {
      viz <- "hgch_choropleth_GnmNum"
    } else {
      viz <- paste0("hgch_", viz_select(), "_", var_viz()$type_viz)
    }

    viz

  })

  # data para graficar ------------------------------------------------------

  data_viz <- reactive({
    req(data_filter())
    req(var_viz())

    data <- data_filter()
    id_ct <- grep("fecha_ct", names(data))
    data <- data[,-id_ct]
    var <- var_viz()$var_viz
    if (length(unique(questions_select()$indicador)) == 2) {
      data <- data |> select({{ var }})
    }
    else {
      data <- data |> select({{ var }}, everything())


      ###########################################################
      #AGR  SECTION , only if required
       temp <- agg_dash_6 |> filter( ind_pregunta ==  questions_select()$ind_pregunta &
                                       ind_subpregunta == questions_select()$ind_subpregunta  &
                                       indicador == questions_select()$indicador & !is.na(agg) &
                                       viz == viz_select())  |>
                            select( viz, agg)

       temp <- temp |> filter(viz == viz_select() )
       if(nrow(temp) > 0) {
          temp <- temp |> filter(viz == viz_select() )
          trad <- temp$agg
          var_calc <- unique(names(data[var_viz()$num_viz]))
          if(ncol(temp) > 1 ) {
            group_var <- unique(names(data[c(1, var_viz()$num_viz-1)  ]))

          }
          else  group_var <- unique(names(data[1]))

        data <- var_aggregation(data = data,
                                       # dic = dic,
                                       agg =trad,
                                       to_agg = var_calc,
                                       name =trad,
                                       group_var =group_var)




       }

       ###########################################################

    }

    vector_names <- lapply( 1:ncol(data), function(i){
      colnames(data)[i]  <- i_(colnames(data)[i], lang())
      print( colnames(data)[i])
    })
      names(data) <- (vector_names)
    data

  })


  viz_theme <- reactive({
    req(viz_select())
    req(slug_trans())
    viz <- viz_select()
    title <- paste0(slug_trans(), collapse = " vs ")
    opts <- list(
      theme = list(
        title = title,
        title_align = "center",
        tiltle_size = 15,
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
        title_size = 17,
        title_weight = 500,
        palette_colors = c("#47BAA6", "#151E42", "#FF4824", "#FFCF06", "#FBCFA4", "#FF3D95", "#B13168")
      )
    )


    if (viz == "map") {
      opts$map_name <- "latamcaribbean_countries"
      opts$theme$palette_colors <- rev(c("#151E42", "#253E58", "#35606F", "#478388", "#5DA8A2", "#7BCDBE", "#A5F1DF"))
      opts$theme$tooltip_template <- paste0("<b>{a}<br/> {b} <br/>")
    }
    opts

  })



  hgch_viz <- reactive({
    req(viz_func())
    req(data_viz())
    req(viz_theme())

    do.call(viz_func(), list(
      data = data_viz(),
      opts = viz_theme()
    ))
  })

  output$viz_hgch <- renderHighchart({
    req(hgch_viz())
    hgch_viz()
  })

  data_down <- reactive({
    req(data_filter())
    df <- data_filter()
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

  output$viz_view <- renderUI({
    req(viz_select())
    height_viz <- 650
    if(!is.null(input$dimension)) height_viz <- input$dimension[2] - 150

    if (viz_select() == "table") {
      DT::dataTableOutput("dt_viz", height = height_viz, width = "100%")
    } else {
      withLoader(
        highchartOutput("viz_hgch", height = height_viz),
        type = "image", loader = "loading_gris.gif")
    }

  })


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


output$debug <- renderPrint({
  list(
   data_viz(),
    #data_questions()$ind_pregunta
    #questions_select()
    names( questions_select())
  )
})




}

shinyApp(ui, server)
