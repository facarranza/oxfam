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
library(shinyjs)
library(httr)
library(jsonlite)
webshot::install_phantomjs()


shorten_url <- function(long_url, access_token) {
  api_url <- "https://api-ssl.bitly.com/v4/shorten"
  headers <- add_headers(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", access_token)
  )
  body <- toJSON(list(long_url = long_url), auto_unbox = TRUE)
  response <- POST(api_url, headers, body = body)
  content(response, "parsed", "application/json")$link
}

ui <- panelsPage(
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="custom.css"),
    tags$script(src="handler.js"),
    tags$script(src = "clipboard.min.js"),
    tags$script(HTML("
      $(document).ready(function() {
        var clipboard = new ClipboardJS('.btn-clipboard');
        clipboard.on('success', function(e) {
          console.info('Accion:', e.action);
          console.info('Texto:', e.text);
          console.info('Disparador:', e.trigger);
          e.clearSelection();
        });
        clipboard.on('error', function(e) {
          console.error('Accion:', e.action);
          console.error('Disparador:', e.trigger);
        });
      });
    "))
  ),
  useShi18ny(),
  useShinyjs(),
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
  shinypanels::modal(id = 'shared_bitly',
                     title = uiOutput("bitly_title"),
                     div(class = "modal-link",
                         uiOutput("bitly_link"),
                         uiOutput("copy_button")
                     )
  ),
  panel(title = ui_("pregunta"),
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

          #verbatimTextOutput("debug"),

          #  shinycustomloader::withLoader(
          uiOutput("country"),
          uiOutput("viz_view")
          #   type = "html", loader = "loader4"
          #   )

        )
  )

)



server <-  function(input, output, session) {

  # url params --------------------------------------------------------------

  url_params <- list(question = NULL, subquestion = NULL, viz = NULL)

  url_par <- reactive({
    shinyinvoer::url_params(url_params, session)
  })

  shared_link <- reactiveValues(short_url = NULL)

  observe({
    if (is.null(lang())) return()
    if (is.null(ques_sel())) return()
    if (is.null(subques_sel$id)) return()
    question <- NULL
    subquestion <- NULL
    viz = NULL

    if (is.null(url_par()$inputs$question)){
      question <- paste0("question=", ques_sel(), "%26")
    } else {
      question <- paste0("question=", url_par()$inputs$question, "%26")
    }
    if (!is.null(url_par()$inputs$subquestion)) subquestion <- paste0("subquestion=", url_par()$inputs$subquestion, "%26")
    else {
      subquestion <- paste0("subquestion=",subques_sel$id, "%26")
    }

    if (!is.null(actual_but$active)) viz <- paste0("viz=",actual_but$active, "%26")

    long_url <- paste0("https://datasketch.shinyapps.io/oxfam_questions/?", gsub("%26", "&",
                                                                                 paste0(question, subquestion, "lang=", lang())))
    print(long_url)
    shared_link$short_url <- shorten_url(long_url, "1ded0052e90265f03473cd1b597f0c45bb83d578")
  })

  # Compartir ---------------------------------------------------------------

  output$compartir <- renderUI({

    HTML(paste0(
      '
      <div class="dropdown-shared">
        <button class="dropbtn-shared">
        <img src="icons/compartir.svg" width="15" height="15">
        </button>
        <div class="dropdown-shared-content">',
      actionButton("fc_click", label = HTML('<img src="icons/facebook.svg" width="15" height="15">')),
      actionButton("tw_click", label = HTML('<img src="icons/twitter.svg" width="15" height="15">')),
      actionButton("bl_click", label = HTML('<img src="icons/link.svg" width="15" height="15">')),
      '</div>
        </div>
      '
    )
    )

  })



  observeEvent(input$fc_click,{
    encoded_short_url <- URLencode(shared_link$short_url)
    share_url <- paste0("https://www.facebook.com/sharer/sharer.php?u=", encoded_short_url)
    print(share_url)
    shinyjs::runjs(sprintf("window.open('%s')", share_url))
  })

  observeEvent(input$tw_click,{
    if (lang() == "es") {
      mensaje <-
        paste0("¿Sabes que hay información en tiempo real sobre el proceso de vacunación en Latinoamérica? 😱👇
Dale clic 👉 ", shared_link$short_url, "
¡Interactúa con estos datos y conviértete en un agente de cambio para &hashtags=VacunasparalaGente!
  &hashtags=DataVacunas
  &hashtags=PandemiaDesigual
  @Vacunas_LAC")
    }
    if (lang() == "en") {
      mensaje <-
        paste0("Do you know what are the impacts of the pandemic that Latin America is currently experiencing? 😱👇
Clic here 👉 ", shared_link$short_url, "
Interact with this data and become an agent of change for &hashtags=Vaccinesforpeople
  &hashtags=DataVaccines
  &hashtags=PandemicUnequal
  @Vacunas_LAC")
    }
    if (lang() == "pt") {
      mensaje <-
        paste0("Você sabe quais são os impactos da pandemia que a América Latina está sofrendo atualmente? 😱👇
Clique aqui 👉 ", shared_link$short_url, "
Interagir com estes dados e tornar-se um agente de mudança para &hashtags=VaccinesforthePeople
  &hashtags=DadosVacinas
  &hashtags=PandemicUnequal
  @Vacunas_LAC")
    }

    tweet_text <- URLencode(mensaje)
    print(tweet_text)
    shinyjs::runjs(sprintf("window.open('%s')", paste0("https://twitter.com/intent/tweet?text=", tweet_text)))

  })

  output$bitly_title <- renderUI({
    i_("bitly_desc", lang())
  })

  output$copy_button <- renderUI({
    tags$button(
      id = "copy_button",
      class = "btn btn-default btn-clipboard",
      icon("copy"),
      `data-clipboard-action` = "copy",
      `data-clipboard-target` = "#bitly_link"
    )
  })

  output$bitly_link <- renderUI({
    req(shared_link$short_url)
    shared_link$short_url
  })

  observeEvent(input$bl_click,{
    shinypanels::showModal("shared_bitly")
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
    if(!is.null(url_par()$inputs$question)) {
      if(url_par()$inputs$question %in% data_questions()$ind_pregunta)
        df_b <- url_par()$inputs$question
      else   df_b <- unique(data_questions()$ind_pregunta)[1]
    }

    else df_b <- unique(data_questions()$ind_pregunta)[1]


    buttons <- dsapptools:::make_buttons(ids = unique(data_questions()$ind_pregunta),
                                         labels = unique(data_questions()[[paste0("pregunta_", lang())]]),
                                         default_active = df_b
    )
    buttons
  })


  ques_sel <- reactive({

   if(!is.null(input$last_click)){
     qs <- input$last_click
   }
    else{
        if(!is.null(url_par()$inputs$question)) {
          if(url_par()$inputs$question %in% data_questions()$ind_pregunta) {
            qs <- url_par()$inputs$question
          } else {
            qs <- unique(data_questions()$ind_pregunta)[1]
          }
        } else{

         qs <- "q_4"
        }
    }
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

    if(!is.null(input$last_click)){
      df_b <- unique(data_subquestions()$ind_subpregunta)[1]
    }
    else {

      if(!is.null(url_par()$inputs$subquestion)) {
        if(url_par()$inputs$subquestion %in% data_subquestions()$ind_subpregunta ) {
          df_b <- url_par()$inputs$subquestion
          subques_sel$id  <- url_par()$inputs$subquestion
        }
        else   df_b <- unique(data_subquestions()$ind_subpregunta)[1]
      }
      else df_b <- unique(data_subquestions()$ind_subpregunta)[1]
    }


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

    if (is.null(sq)){
      if(!is.null(url_par()$inputs$subquestion)) {

        if(url_par()$inputs$subquestion %in% data_subquestions()$ind_subpregunta ) {

          subques_sel$id  <- url_par()$inputs$subquestion
        }
        else   subques_sel$id <- unique(data_subquestions()$ind_subpregunta)[1]
      }
      else subques_sel$id <- unique(data_subquestions()$ind_subpregunta)[1]

    }
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

    c(viz, "table")
  })

  actual_but <- reactiveValues(active = NULL)
  tooltip_info <- reactiveValues(agg = NULL) #tooltip with collapse

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

        #################################################
        #SANKEY SPECIAL SECTION - TODO: optimize in one if
        if( viz_select() %in% ("sankey")){

          if( slug  == "covid_vaccine_agreements" ) {
            d <- d |>
              select(!unidad_id) |>
              group_by(id, unidad) |>
              mutate(unidadp= paste0(unidad, collapse = "-")) |>
              tidyr::separate(unidadp,sep="-",into=c("fabrica","vacuna")) |>
              distinct()
          }
          if( slug  == "doses_delivered_vaccine_donations" ) {
            d <- d |>
              select(!unidad_id) |>
              group_by(id,unidad) |>
              mutate(unidadp= paste0(unidad, collapse = "-")) |>
              tidyr::separate(unidadp,sep="-",into=c("donante","vacuna")) |>
              distinct()
          }

        }
        #################################################

        ################################################
        #BAR SPECIAL CASES
        if( viz_select() %in% c("bar")) {
          if( slug  == "interagency_response_plan_numinneed" ) {
            d$fecha <- as.factor(d$fecha)
          }
        }
        #############################################
      }
      else { #(length(slug) > 1,
        ls <- oxfam_6[[lang()]][slug]
        if (viz_select() %in% c("line", "bar")) {

          id_valor <- grep("valor", names(ls[[1]]))
          names(ls[[1]])[id_valor] <- unique(ls[[1]][[paste0("slug_", lang())]])
          id_valor <- grep("valor", names(ls[[2]]))
          names(ls[[2]])[id_valor] <- unique(ls[[2]][[paste0("slug_", lang())]])

          #TYPE OF JOIN
          if (viz_select() %in% c("bar")) by_types <- c("pais_en", "pais_es", "pais_pt")
          if (viz_select() %in% c("line")) by_types <- c("fecha","pais_en", "pais_es", "pais_pt")

          #################### SPECIAL CASES  TEMPORAL TRANSLATE TO: VAR, IND1, IND2 WITHOUT UNIT TYPE
          if((unique(ls[[1]][[paste0("slug")]]) == "doses_delivered_vaccine_donations" &  unique(ls[[2]][[paste0("slug")]]) == "covid_vaccine_agreements") |
             (unique(ls[[1]][[paste0("slug")]]) == "new_deaths_per_million" &  unique(ls[[2]][[paste0("slug")]]) == "new_cases_per_million") |
             (unique(ls[[1]][[paste0("slug")]]) == "people_fully_vaccinated" &  unique(ls[[2]][[paste0("slug")]]) == "people_vaccinated")) {
            #NOT DATE
            #by_types <-  c("pais_en", "pais_es", "pais_pt")
            ls[[1]] <- ls[[1]] |>
              select(!c(unidad_id,unidad)) |>
              distinct()
            ls[[2]] <- ls[[2]] |>
              select(!c(unidad_id,unidad)) |>
              distinct()

          }
          ######################################

          d <- ls |> purrr::reduce(full_join,
                                   by = by_types,
                                   multiple = "any")

        }
        else { #SCATTER ON PROCESS


          if((unique(ls[[1]][[paste0("slug")]]) == "doses_delivered_vaccine_donations" &  unique(ls[[2]][[paste0("slug")]]) == "covid_vaccine_agreements") |
             (unique(ls[[1]][[paste0("slug")]]) == "new_deaths_per_million" &  unique(ls[[2]][[paste0("slug")]]) == "new_cases_per_million") |
             (unique(ls[[1]][[paste0("slug")]]) == "people_fully_vaccinated" &  unique(ls[[2]][[paste0("slug")]]) == "people_vaccinated")) {
            by_types <- c("pais_en", "pais_es", "pais_pt")
            id_valor <- grep("valor", names(ls[[1]]))
            names(ls[[1]])[id_valor] <- unique(ls[[1]][[paste0("slug_", lang())]])
            id_valor <- grep("valor", names(ls[[2]]))
            names(ls[[2]])[id_valor] <- unique(ls[[2]][[paste0("slug_", lang())]])
            ls[[1]] <- ls[[1]] |>
              select(!c(unidad_id,unidad)) |>
              distinct()
            ls[[2]] <- ls[[2]] |>
              select(!c(unidad_id,unidad)) |>
              distinct()
            d <- ls |> purrr::reduce(full_join,
                                     by = by_types,
                                     multiple = "any")


          }
          if(unique(ls[[1]][[paste0("slug")]]) == "stringency_index" &  unique(ls[[2]][[paste0("slug")]]) == "ghs_index") {
            by_types <- c("pais_en", "pais_es", "pais_pt")
            id_valor <- grep("valor", names(ls[[1]]))
            names(ls[[1]])[id_valor] <- unique(ls[[1]][[paste0("slug_", lang())]])
            id_valor <- grep("valor", names(ls[[2]]))
            names(ls[[2]])[id_valor] <- unique(ls[[2]][[paste0("slug_", lang())]])
            d <- ls |> purrr::reduce(full_join,
                                     by = by_types,
                                     multiple = "any")

          }

          d <- ls |> bind_rows()
          if ("valor" %in% names(d)) {
            d <- d |> tidyr::drop_na(valor)
          }

        }
      }
      ##################################
      #SPECIAL CASES - DATE
      if(viz_select() %in% c("new_cases_per_million","icu_patients_per_million",
                             "reproduction_rate","new_test_per_thousand",
                             "positive_rate","tests_per_case", "new_deaths_per_million",
                             "excess_mortality","excess_mortality_cumulative",
                             "new_deaths_per_million")){

        d <- d |> filter(fecha >="2020-01-01" & fecha <= "2022-12-31")

      }
      ####################################

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
    #if (viz == "map") pais <- "pais_en"
    var_viz <- c(pais, "fecha", "valor")
    type_viz <- "CatDatNum"
    num_viz  <- 3


    if (length(slug) == 1) {
      if (length(unique(df$fecha)) == 1 |
          viz %in% c("map", "bar", "treemap", "sankey")) {

        if(!is.null(df$unidad) & viz %in% c("bar","treemap"))  var_viz <- c(var_viz, "unidad")
        var_viz <- setdiff(var_viz, "fecha")
        if(!is.null(df$unidad))   var_viz <- c(var_viz, "unidad")
        type_viz <- "CatNum"
        num_viz  <- 2

        #############################################
        #SANKEY SPECIAL CASES
        if( viz %in% c("sankey")) {
          if( slug  == "covid_vaccine_agreements" ) {
            var_viz <- c("fabrica", "vacuna","valor")
            type_viz <- "CatCatNum"
            num_viz  <- 3
          }
          if( slug  == "doses_delivered_vaccine_donations" ) {
            var_viz <- c("donante", "vacuna","valor")
            type_viz <- "CatCatNum"
            num_viz  <- 3
          }
          if( slug  == "geopolitics_vaccine_donations" ) {
            var_viz <- c("unidad", pais,"valor")
            type_viz <- "CatCatNum"
            num_viz  <- 3
          }
        }
        #############################################
        #############################################
        #BAR SPECIAL CASES
        if( viz %in% c("bar")) {
          if( slug  == "interagency_response_plan_numinneed" ) {
            var_viz <- c("fecha", pais,"valor")
            type_viz <- "CatCatNum"
            num_viz  <- 3
          }

          if( slug  == "vaccination_approvals_trials" ) {

            var_viz <- c("valor",pais)
            type_viz <- "CatNum"
            num_viz  <- 2

          }

        }
        #############################################

      }
      else if(!is.null(df$unidad) & viz %in% "line")  var_viz <- c(var_viz, "unidad")


    }
    else { # SLUG > 1
      if (viz != "scatter") {
        type_viz <- "DatNumNum"
        num_viz  <- 3
        if (viz == "bar"){
          type_viz <- "CatNumNum"
          num_viz  <- 3
        }

        var_viz <- c("fecha", slug_trans())

        #############################################
        #BAR SPECIAL CASES
        if( viz %in% c("bar")) {
          if( ("doses_delivered_vaccine_donations" %in% slug &   "covid_vaccine_agreements"  %in% slug) |
              ("new_deaths_per_million" %in% slug & "new_cases_per_million" %in% slug ) |
              ("people_fully_vaccinated" %in% slug & "people_vaccinated" %in% slug )) {
            #NOT DATE
            var_viz <- c(pais,slug_trans())
            num_viz  <- 3

          }
        }
        #############################################
      }
      else {#SCATTER
        if( ("doses_delivered_vaccine_donations" %in% slug &   "covid_vaccine_agreements"  %in% slug) |
            ("new_deaths_per_million" %in% slug & "new_cases_per_million" %in% slug ) |
            ("stringency_index" %in% slug & "ghs_index" %in% slug ) |
            ("people_fully_vaccinated" %in% slug & "people_vaccinated" %in% slug )) {
          #NOT DATE
          var_viz <- c(pais,slug_trans())
          num_viz  <- 3
        }
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

      ############SPECIAL CASES
      if( (("doses_delivered_vaccine_donations" %in% questions_select()$indicador &   "covid_vaccine_agreements"  %in% questions_select()$indicador) |
           ("new_deaths_per_million" %in% questions_select()$indicador & "new_cases_per_million" %in% questions_select()$indicador ) |
           ("stringency_index" %in% questions_select()$indicador & "ghs_index" %in% questions_select()$indicador ) |
           ("people_fully_vaccinated" %in% questions_select()$indicador & "people_vaccinated" %in%questions_select()$indicador )) &
          ("scatter"  %in% viz_select())) {
        viz <- paste0("hgch_", "scatter")

      }
      ########################

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
    tooltip_info$agg <- NULL

    if (length(unique(questions_select()$indicador)) == 2) {

      data <- data |> select({{ var }})

      ################################################### SPECIAL CASES, group indicators by agg for scatter and bars
      viz_agg <- agg_dash_6 |>
        filter( ind_pregunta %in%  questions_select()$ind_pregunta &
                  ind_subpregunta %in% questions_select()$ind_subpregunta
                & !is.na(agg) &
                  viz %in% viz_select() ) |>
        select(viz, agg) |>
        filter(viz %in%  viz_select() )

      ###########################################
      #TODO DELETE THIS CODE AFTER THE AGG SCATTER DATABASE AGG HAS BEEN UPDATED

      agg_temp <- NULL

      if( (("doses_delivered_vaccine_donations" %in%  questions_select()$indicador &   "covid_vaccine_agreements"  %in% questions_select()$indicador) |
           ("new_deaths_per_million" %in% questions_select()$indicador & "new_cases_per_million" %in% questions_select()$indicador) |
           ("stringency_index" %in% questions_select()$indicador & "ghs_index" %in% questions_select()$indicador) |
           ("people_fully_vaccinated" %in% questions_select()$indicador & "people_vaccinated" %in% questions_select()$indicador)) &   "scatter" %in% viz_select()){
        viz_agg <- data.frame("agg"=c("mean"))


      }
      ############################################

      if(nrow(viz_agg) > 0) {  #TEST IF NEEDS AGG  OPERATION
        agg <- viz_agg$agg
        tooltip_info$agg <- agg
        group_var <- (unique(names(data[c(1, var_viz()$num_viz-2)  ])))
        var_calc <- unique(names(data[c(2,3)])) ### DATA ESTATICA

        ### TODO  UPDATE WITH DSAAPPTOOLS
        if(agg=="mean"){
          data <- data |>
            group_by(!!sym(group_var)) |>
            summarise(x = mean(!!sym(var_calc[1]), na.rm = T), y = mean(!!sym(var_calc[2]), na.rm = T))
        }
        else{
          if(agg=="sum") {
            data <- data |>
              group_by(!!sym(group_var)) |>
              summarise(x = sum(!!sym(var_calc[1]), na.rm = T), y = sum(!!sym(var_calc[2]), na.rm = T))
          }

        }
        names(data) <- c(agg, var_calc[1],var_calc[2])
        #names(data) <- c(agg, paste(var_calc[1], "  -", i_(agg,lang()),""), paste(var_calc[2], " ", i_(agg,lang()),""))
      }

      ###############################################################
    }
    else {

      viz_agg <- agg_dash_6 |>
        filter( ind_pregunta %in%  questions_select()$ind_pregunta &
                  ind_subpregunta %in% questions_select()$ind_subpregunta  &
                  indicador%in% questions_select()$indicador & !is.na(agg) &
                  viz %in% viz_select() ) |>
        select(viz, agg) |>
        filter(viz %in%  viz_select() )


      if(nrow(viz_agg) > 0) { #TEST IF NEEDS AGG  OPERATION
        data <- data |> select({{ var }}, everything())
        agg <- viz_agg$agg
        tooltip_info$agg <- agg
        var_calc <- unique(names(data[var_viz()$num_viz]))
        if(ncol(viz_agg) > 1 ) {
          group_var <- unique(names(data[c(1, var_viz()$num_viz-1)  ]))

        }
        else  group_var <- unique(names(data[1]))


        if("vaccination_approvals_trials" %in% questions_select()$indicador){

          group_var <- unique(names(data[c(1,2)]))
          data$valor <- as.character(data$valor)

          data_temp1 <- var_aggregation(data = data,
                                        # dic = dic,
                                        agg = agg,
                                        to_agg = var_calc,
                                        name = agg,
                                        group_var = group_var)

          data <- data_temp1 |> tidyr::pivot_wider(names_from = valor, values_from = count, names_prefix="stage_",values_fill = 0)
          data$count <- data$stage_1 + data$stage_2 + data$stage_3
          data <- data |> select(group_var[2], count, stage_1, stage_2 ,stage_3)

        }
        else {

          data_temp1 <- var_aggregation(data = data,
                                        # dic = dic,
                                        agg = agg,
                                        to_agg = var_calc,
                                        name = agg,
                                        group_var = group_var)

          #REQUIRED MIN MAX -  STEP PROGRESS - REQUIRED: UNIT
          unidad <- NULL
          if(!is.null(data$unidad)) unidad <- unique(data$unidad_id)
          data_temp2 <- data |> group_by(across(sym(group_var))) |> summarise(min_date = min(!!sym("fecha")), max_date = max(!!sym("fecha")))
          data_temp2$min_date <- as.character(data_temp2$min_date)
          data_temp2$max_date <- as.character(data_temp2$max_date)
          data <- data_temp1 |> left_join( data_temp2)

          if(!is.null(unidad)) {
            if(length(unidad) == 1) data$unidad <- unidad
          }
        }
      }
      else{
        data <- data |> select({{ var }})
        if(viz_select() %in% c("line","bar","treemap")) {
          tooltip_info$agg <- "None"
        }
      }
    }
    colnames(data)  <- i_(colnames(data), lang())
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
    if(!is.null(tooltip_info$agg)) opts$collapse_rows = T

    if(viz=="line" & "stringency_index" %in% questions_select()$indicador){
      opts$y_max <- 100
    }
    if (viz == "map") {
      opts$map_name <- "world_countries_latin_america_caribbean"
      opts$theme$palette_colors <- rev(c("#151E42", "#253E58", "#35606F", "#478388", "#5DA8A2", "#7BCDBE", "#A5F1DF"))
      pais_bold <- paste0("<b>",i_("pais",lang()), ": </b>")
      pais_detail <-  paste0("{",i_("pais",lang()), "}")
      if(!is.null(tooltip_info$agg)) {
        value_bold  <-   paste0("<b>",i_(tooltip_info$agg,lang()), ": </b>")
        value_detail <-  paste0("{",i_(tooltip_info$agg,lang()), "}")
      }
      else{
        value_bold  <-   paste0("<b>",i_("valor",lang()), ": </b>")
        value_detail <-  paste0("{",i_("valor",lang()), "}")
      }
      fecha_min_bold <- paste0("<b>",i_("min_date",lang()), ": </b>")
      fecha_min_detail <-  paste0("{","mindate", "}")
      tooltip <- paste0(pais_bold,  pais_detail, "<br>", value_bold ,  value_detail,"<br>")
      opts$theme$tooltip_template <- {tooltip}

    }

    opts

  })



  hgch_viz <- reactive({
    req(viz_func())
    req(data_viz())
    req(viz_theme())

    ############################# SPECIAL CASE
    if( (("doses_delivered_vaccine_donations" %in%  questions_select()$indicador &   "covid_vaccine_agreements"  %in% questions_select()$indicador) |
         ("new_deaths_per_million" %in% questions_select()$indicador & "new_cases_per_million" %in% questions_select()$indicador) |
         ("stringency_index" %in% questions_select()$indicador & "ghs_index" %in% questions_select()$indicador) |
         ("people_fully_vaccinated" %in% questions_select()$indicador & "people_vaccinated" %in% questions_select()$indicador)) &   "scatter" %in% viz_select()){

      data <- data_viz()
      colnames(data)  <- c(i_("pais", lang()),names(data[2]),names(data[3]))
      do.call(viz_func(), list(
        data = data,
        opts = viz_theme(),
        var_num = c(names(data[2]),names(data[3])),
        var_cat =  c(i_("pais", lang()))
      ))
      #############################
    }
    else{
      do.call(viz_func(), list(
        data = data_viz(),
        opts = viz_theme()
      ))
    }
  })

  output$viz_hgch <- renderHighchart({
    req(hgch_viz())
    hgch_viz()
  })


  # TODO:
  data_down_table <- reactive({
    req(data_filter())
    df <- data_filter()
    if( (("doses_delivered_vaccine_donations" %in%  questions_select()$indicador &   "covid_vaccine_agreements"  %in% questions_select()$indicador) |
         ("new_deaths_per_million" %in% questions_select()$indicador & "new_cases_per_million" %in% questions_select()$indicador) |
         ("stringency_index" %in% questions_select()$indicador & "ghs_index" %in% questions_select()$indicador) |
         ("people_fully_vaccinated" %in% questions_select()$indicador & "people_vaccinated" %in% questions_select()$indicador))){
      df[,-1]
    }
    else{
      var_select <- c(c("id", paste0("slug_", lang()),
                        paste0("pais_", lang())), "fecha", "valor")

      if ("unidad" %in% names(df)) {
        var_select <- c(var_select, "unidad_id")
      }

      df <- df[,var_select]

      if ("unidad_id" %in% names(df)) {
        df <-  df |> rename(unidad = unidad_id)
      }

      names_tr <- i_(names(df), lang = lang())
      names(df) <- names_tr


      df[,-1]
    }
  })


  output$dt_viz <- DT::renderDataTable({
    req(viz_select())
    if (viz_select() != "table") return()
    req(data_down_table())
    df <- dplyr::as_tibble(data_down_table())
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
      DT::dataTableOutput("dt_viz", height = height_viz, width = "500px")
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
                                   element = reactive(data_down_table()),
                                   formats = c("csv", "xlsx", "json"))
    dsmodules::downloadImageServer("download_viz",
                                   element = reactive(hgch_viz()),
                                   lib = "highcharter",
                                   formats = c("jpeg", "pdf", "png", "html"),
                                   file_prefix = "plot")
  })


  output$debug <- renderPrint({
    list(
      #data_filter()
      #data_viz()
      #data_questions()$ind_pregunta
      #questions_select()
      #  names( questions_select())
    )
  })




}

shinyApp(ui, server)
