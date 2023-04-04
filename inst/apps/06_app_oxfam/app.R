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
          img(src= 'logos_es.svg', style = "border-top: 1px solid #252525;",
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
    df <- req(data_slug())
    slug <- unique(questions_select()$indicador)
    paste_fnc <- function (x, collapse = "") {
      paste0(trimws(unique(x)), collapse = collapse)
    }
    
    if(viz_select() %in% c("bar","line") & "school_closures" %in% slug) {
      if (!is.null(input$country)) {
        pais <- paste0("pais_", lang())
        df <- df |> filter(!!sym(pais) %in% input$country)
      }
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
    
    df
  })
  

  # Variables a visualizar --------------------------------------------------
  
  
  var_viz <- reactive({
    req(viz_select())
    req(data_filter())
    slug <- unique(questions_select()$indicador)

    viz <- viz_select()
    viz
  })
  

  output$debug <- renderPrint({
    list(
      var_viz()
    )
  })

  
  
  
}

shinyApp(ui, server)
