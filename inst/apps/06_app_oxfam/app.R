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
    tags$script(src="handler.js"),
    tags$script(src="handler2.js"),
    tags$script(src="handler3.js")
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
  panel(title = ui_("data_filter"),
        id = "controls-style",
        collapse = FALSE,
        can_collapse = FALSE,
        width = 285,
        body = div(
          div(id = "myDiv", style = "font-family: 'IBM Plex Sans'; font-weight: 500; font-size: 14px; line-height: 18.2px;,  background-color:#252525;" , ui_("pregunta")),
          verbatimTextOutput("choices_print"),
          uiOutput("generalFilters")
        ),

        footer = tags$a(
          href="https://www.datasketch.co", target="blank",
          img(src= 'logos_es.svg', style = "border-top: 1px solid #252525;",
              align = "left", width = 300, height = 80))
  ),
  panel(title = ui_("subpregunta"),
        id = "controls-style2",

        can_collapse = FALSE,
        width = 285,
        body = div(

          #shinycustomloader::withLoader(
            uiOutput("generalsubFilters")
           # type = "html", loader = "loader4"
          #)

        )
  ),
  panel(title = ui_("data_viz"),
        id = "viz-style",
        #width = 879,
        header_right = div(style="display:flex;align-items: flex-end;",
          class = "head-viz",
          div(style = "display:flex;",
              class = "viz-style",
              uiOutput("viz_icons")),
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

  config <- i18nConfig(i18n)
  #adding filters

  observeEvent(lang(),{
    shinyjs::delay(500, uiLangUpdate(input$shi18ny_ui_classes, lang()))
  })

  #TODO create objetcs that group variables
  click_viz <- reactiveValues(id = NULL)
  click_sub <- reactiveValues(value = NULL)
  click_viz_sub <- reactiveValues(id = NULL)
  Indicador <- reactiveValues( value = NULL)
  indicador_title <- reactiveValues( value = NULL)
  Unidad <-  reactiveValues( value = NULL)
  actual_but <- reactiveValues(active = NULL)
  country_url <-  reactiveValues(paises = NULL)
  question_url_def <-  reactiveValues(val= NULL)
  subquestion_url_def <-  reactiveValues(val= NULL)
  title_x_axis <- reactiveValues(val= NULL)
  title_y_axis <- reactiveValues(val= NULL)


  #genera la data con base a las preguntas elegidas y el tipo de idioma
  get_basic_lang_data <- reactive({
    req(quest_choose())
    req(quest_choose_sub())

    temp <-  NULL
    question <- quest_choose()
    subquestion <- quest_choose_sub()
    indicador_title$value <- NULL
    print("vaaaal")
    print(names(oxfam_6$en$new_vaccinations))
   # return()
    if(lang()=="en"){

      indicador <- questions_dash_6 |> filter(pregunta_en %in% question & subpregunta_en %in% subquestion ) |>  select(indicador)
      Indicador$value <- indicador
      indicador_title$value <- slug_translate |> filter(slug %in% indicador$indicador) |> select(slug_en)  |> rename(slug = slug_en)
      print("indicador")
      print(indicador)
      print("indicador_title$value ")
      print(indicador_title$value)
      print(oxfam_6$en[as.vector(indicador$indicador)])
      temp <- plyr::ldply( 1:length(indicador$indicador), function(i){

      t  <- as.data.frame(oxfam_6$en[as.vector(indicador$indicador[i])])
        if(ncol(t) == 9) {

         colnames(t) <-  c("id", "slug", "slug_en","fecha", "pais_es", "pais_en", "pais_pt","valor","unidad")
        }
     else{
       if(ncol(t) == 11) {

        colnames(t) <-  c("id", "slug", "slug_en","fecha", "pais_es", "pais_en", "pais_pt","valor","unidad_id","unidad","fecha_ct")
        }
        else  {
         colnames(t) <-  c("id", "slug", "slug_en","fecha", "pais_es", "pais_en", "pais_pt","valor")
        }
      }

        temp <- rbind(temp,t)
      })


    }
    else{
      if(lang()=="es"){
        indicador <- questions_dash_6 |> filter(pregunta_es %in% question & subpergunta_es %in% subquestion ) |>  select(indicador)
        Indicador$value <- indicador
        indicador_title$value <- slug_translate |> filter(slug %in% indicador$indicador) |> select(slug_es) |> rename(slug = slug_es)
        # temp <- indicador

        temp <- lapply( 1:length(indicador$indicador), function(i){
          t  <- as.data.frame(oxfam_6$es[as.vector(indicador$indicador[i])])
          if(ncol(t)==9)
            colnames(t) <-  c("id", "slug", "slug_es","fecha", "pais_es", "pais_en", "pais_pt","valor","unidad")
          else {

            if(ncol(t) == 11) {
              colnames(t) <-  c("id", "slug", "slug_es","fecha", "pais_es", "pais_en", "pais_pt","valor","unidad_id","unidad","fecha_ct")

            }
            else
            colnames(t) <-  c("id", "slug", "slug_es","fecha", "pais_es", "pais_en", "pais_pt","valor")
          }
          temp <- rbind(temp,t)
        })
      }
      else{

        if(lang()=="pt"){
          indicador <- questions_dash_6 |> filter(pregunta_pt %in% question & subpregunta_pt %in% subquestion ) |>  select(indicador)
          Indicador$value <- indicador
          print("indicador")
          print(indicador)

          indicador_title$value <- slug_translate |> filter(slug %in% indicador$indicador) |> select(slug_pt) |> rename(slug = slug_pt)
          print("indicador_title$value ")
          print(indicador_title$value)

          temp <- lapply( 1:length(indicador$indicador), function(i){
           t  <- as.data.frame(oxfam_6$pt[as.vector(indicador$indicador[i])])
            if(ncol(t)==9)
              colnames(t) <-  c("id", "slug", "slug_pt","fecha", "pais_es", "pais_en", "pais_pt","valor","unidad")
            else{
              if(ncol(t) == 10) {
                colnames(t) <-  c("id", "slug", "slug_pt","fecha", "pais_es", "pais_en", "pais_pt","valor","unidad_id","unidad","fecha_ct")

            }
              else
              colnames(t) <-  c("id", "slug", "slug_pt","fecha", "pais_es", "pais_en", "pais_pt","valor")
            }
            temp <- rbind(temp,t)

          })

        }

      }

    }
    temp
  })

  #filtro de pereguntas
  sel_question <-reactive({
    if(lang() == "es") {
      unique(questions_dash_6$pregunta_es)

    } else {
      if(lang() == "en") {
        unique(questions_dash_6$pregunta_en)
      }
      else { if(lang() == "pt")
        unique(questions_dash_6$pregunta_pt)
      }
    }
    # ui_(unique(oxfam_one$es$new_vaccinations$pais), lang = lang())
  })

  #Genera botones de selección de preguntas
  output$generalFilters <- renderUI({
    req( sel_question_url())
    req(question_url_def$val)

    #req(quest_choose())
    buttons <- dsapptools:::make_buttons( sel_question(), labels = sel_question(), default_active =  sel_question_url())

    val <- as.numeric(question_url_def$val)

    buttons[[val]] <- gsub("needed", "needed basic_active",  buttons[[val]])
    buttons[[val]] <- htmltools::HTML(paste0(paste( buttons[[val]], collapse = '')))
    buttons
    #question_buttons_2(c( "Inspecciones","Aprehensiones"),
                  #   c( "Visitas de control","Aprehensiones" )


  })

  output$generalsubFilters <- renderUI({
    req(sel_subquestion_url())
    buttons <- dsapptools:::make_buttons( sel_subquestion(), labels = sel_subquestion(), class="needed_sub", class_active = "basic_active_sub", default_active =   sel_subquestion_url())

    val <- as.numeric(subquestion_url_def$val)

    buttons[[val]] <- gsub("needed_sub", "needed_sub basic_active_sub",  buttons[[val]])
    buttons[[val]] <- htmltools::HTML(paste0(paste( buttons[[val]], collapse = '')))
    buttons
  })


  observe({
    click_sub$value <-  input$last_click_sub
  })


  quest_choose <- reactive({
    last_btn <- input$last_click
    click_sub$value <- NULL
    #click_viz$id <- NULL
    if (is.null(last_btn)){
      if(lang() == "es") {
        last_btn <- sel_question_url()  #unique(questions_dash_6$pregunta_es)[1]

      } else {
        if(lang() == "en") {
          last_btn <- sel_question_url() #unique(questions_dash_6$pregunta_en)[1]
        }
        else { if(lang() == "pt")
          last_btn <- sel_question_url() #unique(questions_dash_6$pregunta_pt)[1]
        }
      }

    }

    last_btn

  })





  quest_choose_sub <- reactive({
    req(quest_choose())
    last_btn <- click_sub$value
   # click_viz_sub$id <- NULL
    if (is.null(last_btn)){
      if (is.null(last_btn)){
        if(lang() == "es") {
         # temp_q <-  questions_dash_6 |> filter(pregunta_es == quest_choose()) |> select(subpergunta_es)
          last_btn <-sel_subquestion_url() # unique(temp_q$subpergunta_es)[1]

        } else {
          if(lang() == "en") {
             last_btn <- sel_subquestion_url() #unique(temp_q$subpregunta_en)[1]
          }
          else { if(lang() == "pt")
            # temp_q <-  questions_dash_6 |> filter(pregunta_pt == quest_choose()) |> select(subpregunta_pt)
            last_btn <-  sel_subquestion_url()  #unique(temp_q$subpregunta_pt)[1]
          }
        }

      }

    }

    last_btn

  })



  sel_subquestion <-reactive({
    req(quest_choose())


    if(lang() == "es") {
      t <- questions_dash_6 |> dplyr::filter(pregunta_es %in% quest_choose()) |> select(subpergunta_es)
      unique(t$subpergunta_es)

    } else {
      if(lang() == "en") {
        t <- questions_dash_6 |> dplyr::filter(pregunta_en %in% quest_choose()) |> select(subpregunta_en)
        unique(t$subpregunta_en)
      }
      else { if(lang() == "pt")
        t <- questions_dash_6 |> dplyr::filter(pregunta_pt %in% quest_choose()) |> select(subpregunta_pt)
       unique(t$subpregunta_pt)
      }
    }
    # ui_(unique(oxfam_one$es$new_vaccinations$pais), lang = lang())
  })



  question_label <- reactive({
    i_(c("pregunta"), lang = lang())
  })

  subquestion_label <- reactive({
    i_(c("subpregunta"), lang = lang())
  })



  sel_question_url <- reactive({

    query <- parseQueryString(session$clientData$url_search)
    temp <- stringr::str_to_title(query[["question"]])
    temp2 <- NULL
     if(!is.null(temp) & !identical(temp, character(0))) {
      # questions_dash_6$q1 <-  as.numeric(as.factor(questions_dash_6$pregunta_en))

      questions_dash_6 <-   questions_dash_6 |> mutate(q1 = case_when(
        pregunta_es == "¿Cómo ha sido el proceso de vacunación en América Latina y el Caribe?"   ~ 1,
        pregunta_es == "¿Cómo ha evolucionado el COVID-19 en los países de la región?"       ~ 2,
        pregunta_es == "¿Cuál ha sido el comportamiento de los fallecimientos por COVID-19 en los países entre 2020 y 2022?"  ~ 3,
        pregunta_es == "¿Qué efectos ha tenido el COVID-19 en cada país?"     ~ 4,
        pregunta_es == "¿Cuáles compras, desarrollos y aprobaciones de vacunas han hecho los países?"    ~ 5,
        pregunta_es == "¿Qué donaciones y apoyos financieros han obtenido los países para responder a la pandemia?"   ~ 6
      ))

      question_url_def$val = temp

      if(lang()=="en" | is.null(lang())) {
        temp2 <- questions_dash_6 |> filter(q1 %in% temp)  |> select(pregunta_en) |> rename(pregunta =  pregunta_en)

      }
      if(lang()=="es") {
        temp2 <- questions_dash_6 |> filter(q1 %in% temp)  |> select(pregunta_es) |> rename(pregunta =  pregunta_es)
      }
      if(lang()=="pt") {
        temp2 <- questions_dash_6 |> filter(q1 %in% temp) |>   select(pregunta_pt)  |> rename(pregunta =  pregunta_pt)
      }

      #temp2
      i_(unique(temp2$pregunta), lang=lang())
    }

    else{

      question_url_def$val =1

      if(lang() == "es") {
        unique(questions_dash_6$pregunta_es)[1]

      } else {
        if(lang() == "en") {
          unique(questions_dash_6$pregunta_en)[1]
        }
        else { if(lang() == "pt")
          unique(questions_dash_6$pregunta_pt)[1]
        }
      }}


  })


  sel_subquestion_url <- reactive({
    req(quest_choose())
    query <- parseQueryString(session$clientData$url_search)
    temp <- stringr::str_to_title(query[["subquestion"]])
    temp0 <- stringr::str_to_title(query[["question"]])
    if(!is.null(temp) & !identical(temp, character(0))) {

      # questions_dash_6$q2 <-  as.numeric(as.factor(questions_dash_6$subpregunta_en))

      questions_dash_6 <-  questions_dash_6 |> mutate( q2 = case_when(
        subpergunta_es == "¿Cuántas personas han recibido una dosis de vacuna contra el COVID-19 en los países de la región?" ~ 1,
        subpergunta_es == "¿Cuántas personas han recibido todas las dosis prescritas por el protocolo de vacunación inicial contra el COVID-19 en los países de la región?" ~ 2,
        subpergunta_es == "¿Cuántas personas han recibido al menos una dosis de vacuna contra el COVID-19 en los países de la región?"   ~ 3,
        subpergunta_es == "¿Cuántas dosis de refuerzo de la vacuna fueron administradas por cada cien personas?"                             ~ 4,
        subpergunta_es == "¿Cuántas personas recibieron al menos una dosis de vacuna entre 100 personas?"                                    ~ 5,
        subpergunta_es == "¿Cuántas personas completamente vacunadas hay en comparación con las personas que recibieron mínimo una dosis?"   ~ 6,
        subpergunta_es =="¿Cuántos casos de COVID-19 por millón de habitantes se confirmaron por día?"                                       ~ 1,
        subpergunta_es =="¿Cuántas personas han sido internadas en Unidades de Cuidado Intensivo?"                                               ~ 2,
        subpergunta_es =="¿Cuál es la diferencia entre el número de pacientes en Unidades de Cuidado Intensivo y en hospitales por millón?"        ~ 3,
        subpergunta_es =="¿Cuál es la admisión semanal en hospitales por millón en comparación con la admisión en Unidades de Cuidado Intensivo?"    ~ 4,
        subpergunta_es =="¿Cuántas nuevas infecciones en promedio ha causado un solo individuo infectado de COVID-19 en cada país?"                       ~ 5,
        subpergunta_es =="¿Cuántas pruebas de COVID-19 en promedio se han hecho entre mil habitantes de cada país?"                                      ~ 6,
        subpergunta_es == "¿Cuál es la tasa de pruebas positivas de COVID-19 por país?"                                                                      ~ 7,
        subpergunta_es =="¿Cuánto ha sido el total de pruebas dividido entre el número de casos confirmados por país?"                                       ~ 8,
        subpergunta_es =="¿Cuántas muertes a causa de COVID-19 por millón de habitantes se reportaron en cada país, incluyendo presuntas muertes?"          ~ 1,
        subpergunta_es =="¿Cuál es la diferencia de porcentaje entre el número reportado de muertes semanales o mensuales entre 2020 y 2022 y el número proyectado de muertes para el mismo período con base en años anteriores de los países de América Latina y el Caribe?" ~ 2,
        subpergunta_es =="¿Cuál es la diferencia acumulada entre el número informado de muertes desde el 1 de enero de 2020 y el número proyectado de muertes para el mismo período con base en años anteriores de los países de América Latina y el Caribe?" ~ 3,
        subpergunta_es =="¿Cuántas muertes en exceso hubo en total en los países respecto a las proyectadas en años anteriores?" ~ 4,
        subpergunta_es =="¿Cuál es la correlación entre las nuevas muertes por millón y el acumulado en el exceso de mortalidad?"   ~ 5,
        subpergunta_es =="¿Cuál es la diferencia entre las nuevas muerte y los nuevos casos  por millón en cada país entre 2020 y 2022?" ~ 6,
        subpergunta_es =="¿Cuál fue el Índice de Rigurosidad de los países a lo largo de la pandemia por establecer cierres de escuelas, cierres de lugares de trabajo y prohibiciones de viaje, entre otras medidas?" ~ 1,
        subpergunta_es =="¿Cuántas personas con necesidad de asistencia y protección humanitarias fueron asistidas por el Plan de Respuesta Interagencial de Naciones Unidas en los países?"  ~ 2,
        subpergunta_es =="¿Cuántas personas no fueron vacunadas de otras enfermedades debido al COVID-19?"                                                                                    ~ 3,
        subpergunta_es =="¿Cuál ha sido el estado de operación de las instituciones educativas debido a las medidas de los gobiernos para mitigar la pandemia?"                               ~ 4,
        subpergunta_es =="¿Cuál es el Índice de Seguridad de Salud Global de los países, de acuerdo con su seguridad sanitaria y otras capacidades relacionadas para responder a epidemias y pandemias?" ~ 5,
        subpergunta_es =="¿Cuál es la correlación entre el promedio del Índice de Rigurosidad y el Índice de Seguridad de Salud Global de los países?"                                                    ~ 6,
        subpergunta_es =="¿Cuántas dosis recibieron los países de la región a partir de acuerdos con los fabricantes?"                                                                                        ~ 1,
        subpergunta_es =="¿Cuáles fueron los fabricantes con los que los países establecieron acuerdos para obtener vacunas?"                                                                               ~2,
        subpergunta_es =="¿Cuáles son los precios por dosis en los países según las investigaciones periodísticas?"                                                                                         ~ 3,
        subpergunta_es =="¿En qué fase de ensayo clínico están las vacunas en los países de la región?"                                                                                                     ~ 4,
        subpergunta_es =="¿Cuántos proyectos de vacunas contra el COVID-19 desarrollaron algunas instituciones en países de América Latina y el Caribe?"                                                    ~ 5,
        subpergunta_es =="¿Cuánto apoyo financiero en dólares recibieron los países por parte de la Alianza para las Vacunas Gavi y el Banco Mundial?"                                                      ~ 1,
        subpergunta_es =="¿Cuánta asignación de financiación de proyectos en dólares han recibido los países por parte del Fondo Central para la Acción en Casos de Emergencia (CERF) de OCHA?"             ~ 2,
        subpergunta_es =="¿Qué países donaron vacunas a América Latina y el Caribe?"                                                                                                                        ~ 3,
        subpergunta_es =="¿Cuántas dosis donadas recibieron los países de América Latina y el Caribe?"                                                                                                      ~ 4,
        subpergunta_es =="¿Cuántas dosis donaron China y Estados Unidos a los países de la región y cómo están distribuidas?"                                                                               ~ 5,
        subpergunta_es =="¿Cuál es la diferencia entre las dosis obtenidas por acuerdos con fabricantes y por donaciones?"  ~ 6
      ))
      subquestion_url_def$val <- temp

      if(lang()=="en" | is.null(lang())) {

        data_t <- questions_dash_6 |> filter(pregunta_en %in%  unique(quest_choose())[1] )

        temp2 <- data_t |> filter(q2 %in% temp)  |> select(subpregunta_en) |> rename(subpregunta =  subpregunta_en)

        if(nrow(temp2)==0){

         temp2 <- data_t  |> select(subpregunta_en) |> rename(subpregunta =  subpregunta_en) |> head(1)
         subquestion_url_def$val <- 1



        }

      }
      if(lang()=="es") {

        data_t <-questions_dash_6 |> filter(pregunta_es  %in%  unique(quest_choose())[1])
        temp2 <- data_t |> filter(q2 %in% temp)  |> select(subpergunta_es) |> rename(subpregunta =  subpergunta_es)

        if(nrow(temp2)==0){
          temp2 <- data_t  |> select(subpergunta_es) |> rename(subpregunta =  subpergunta_es) |> head(1)
          subquestion_url_def$val <- 1


        }
      }
      if(lang()=="pt") {
        data_t <-questions_dash_6 |> filter(pregunta_pt ==  unique(quest_choose())[1] )
        temp2 <- data_t |> filter(q2 %in% temp)  |> select(subpregunta_pt) |> rename(subpregunta =  subpregunta_pt)
        if(nrow(temp2)==0){
          temp2 <- data_t  |> select(subpregunta_pt) |> rename(subpregunta =  subpregunta_pt) |> head(1)
          subquestion_url_def$val <- 1


        }
      }

      temp <- unique(temp2$subpregunta)
    }
    else{
      subquestion_url_def$val <- 1
      if(lang() == "es") {

        data_t <-questions_dash_6 |> filter(pregunta_es  %in%   unique(quest_choose())[1])
          temp2 <- data_t  |> select(subpergunta_es) |> rename(subpregunta = subpergunta_es)
          temp <- unique(temp2$subpregunta)[1]


        } else {
          if(lang() == "en") {
            data_t <-questions_dash_6 |> filter(pregunta_en  %in%   unique(quest_choose())[1])
            temp2 <- data_t  |> select(subpregunta_en) |> rename(subpregunta =  subpregunta_en)
            temp <- unique(temp2$subpregunta)[1]
          }
          else {
            if(lang() == "pt"){
             unique(questions_dash_6$pregunta_pt)[1]
            data_t <-questions_dash_6 |> filter(pregunta_pt  %in%   unique(quest_choose())[1])
            temp2 <- data_t  |> select(subpregunta_pt) |> rename(subpregunta =  subpregunta_pt)
            temp <- unique(temp2$subpregunta)[1]
            }
          }
        }

      }


  })




  output$country <- renderUI({
    req(Indicador$value)
    req(actual_but$active)
    print("Indicador$value")
    print(Indicador$value)
    print(actual_but$active)
    if(actual_but$active %in% c("barras","linea") &  Indicador$value %in% c("school_closures")) {
        default_select <- NULL
        default_select <- NULL

        shiny::selectizeInput("country", label= i_("pais",lang()), choices=  sel_country(), selected= sel_country()[1],  multiple =TRUE,
                              options = list(
                                placeholder = "All", plugins=list("remove_button","drag_drop"))
        )
    }




  })


  sel_country <-reactive({
    req(data_prep())
    dta <- bind_rows(data_prep())

    if(lang() == "es"){
      dta <-   dta |> select(!c(pais_en,pais_pt)) |> rename(pais = pais_es)

    }
    if(lang() == "en"){
      dta <-   dta |> select(!c(pais_es,pais_pt)) |> rename(pais = pais_en)
    }
    if(lang( )== "pt"){
      dta <-   dta |> select(!c(pais_es,pais_en)) |> rename(pais = pais_pt)


    }
    unique(dta$pais)
    # ui_(unique(oxfam_one$es$new_vaccinations$pais), lang = lang())
  })



  sel_country_lang <-reactive({
    req(data_prep())
    dta <- bind_rows(data_prep())

    if(lang() == "es"){
      dta <-   dta |> select(!c(pais_pt)) |> rename(pais_lang = pais_es) |> rename(pais_en = pais_en)

    }
    if(lang() == "en"){
      dta <-   dta |> select(!c(pais_es,pais_pt)) |> rename(pais_lang = pais_en) |> mutate(pais_en = pais_lang)
    }
    if(lang( )== "pt"){
      dta <-   dta |> select(!c(pais_en)) |> rename(pais_lang = pais_pt,pais_en = pais_en)


    }
    print("dta")
    print( dta)
    dta |> select(pais_en, pais_lang) |> distinct()
    # ui_(unique(oxfam_one$es$new_vaccinations$pais), lang = lang())
  })


  data_prep <- reactive({
    req(quest_choose())
    req(quest_choose_sub())
    data  <- get_basic_lang_data() #todo; epserar nuevos filtros
    # dic <- homodatum::create_dic(data)
    # names(data) <- dic$id
    # pais_temp <-  input$Country
    # if(is.null(input$Country)) pais_temp <- "All"
    # #print(pais_temp)
    # var_inputs <-  list( pais = as.vector(pais_temp)) #, fecha= input$data_range) #list(input$Indicator,input$Country)
    # data_result <- data_filter(data.frame(data),
    #                            dic,
    #                            var_inputs = var_inputs,
    #                            special_placeholder = "All"
    # )
    #
    # data_result <-  data_result |> filter(fecha >= input$data_range[1]  & fecha <= input$data_range[2] )
    # # checking results

    data

  })

  #########################################################

  data_table <- reactive({
    req(data_prep())
    dta <- bind_rows(data_prep())

    if(lang() == "es"){
      dta <-   dta |> select(!c(pais_en,pais_pt)) |> rename(pais = pais_es, slug_en=slug_es)

   }
    if(lang() == "en"){
      dta <-   dta |> select(!c(pais_es,pais_pt)) |> rename(pais = pais_en)
    }
    if(lang( )== "pt"){
      dta <-   dta |> select(!c(pais_es,pais_en)) |> rename(pais = pais_pt, slug_en=slug_es)

    }

    if(  "covid_vaccine_agreements"  %in% Indicador$value  & nrow(Indicador$value)==1) {

      # a |> tidyr::pivot_wider(names_from=unidad)
      slug_id <-  paste(i_("slug",lang()),"id",sep="_")
      print(colnames(dta))
      print("dta innnn ")
      # print(dta)
      # return()
      dta <- dta |> select(id, slug, slug_en, fecha, pais, valor, unidad_id)  |> distinct() |> mutate(unidadp= paste0(unidad_id, collapse = "-")) |>
        tidyr::separate(unidadp,sep="-",into=c("fabrica","vacuna")) |> ungroup() |> select(!unidad_id) |> distinct()
      print(names(dta))
      #return()
      #|> tidyr::pivot_wider( names_from = "unidad_id", values_from="valor")# mutate(unidadp= paste0(unidad, collapse = "-")) |> tidyr::separate(unidadp,sep="-",into=c("fabrica","vacuna"))

      # print(dta)
      # return()
        lang_names <-  c("id",slug_id ,i_("slug",lang()),i_("fecha",lang()), i_("pais",lang()),  i_("valor",lang()), i_("fabrica",lang()), i_("vacuna",lang()))
        # print("dta innnn ") i_("fabrica",lang()), i_("vacuna",lang()))
      # print("dta innnn ")
      print(names(dta))


      colnames(dta) <-  lang_names

    } else {



    slug_id <-  paste(i_("slug",lang()),"id",sep="_")
    lang_names <-  c("id",slug_id ,i_("slug",lang()),i_("fecha",lang()), i_("pais",lang()), i_("valor",lang()), i_("unidad_id",lang()),
                     i_("unidad",lang()), i_("fecha_ct",lang()))

     lang_names_2 <-  c("id",slug_id ,i_("slug",lang()),i_("fecha",lang()), i_("pais",lang()), i_("valor",lang()), i_("unidad",lang()))

     if(ncol(dta)==9)  colnames(dta) <-  lang_names
      else{
      if(ncol(dta)==11) {
        colnames(dta) <-  lang_names
      }
      else {
        colnames(dta) <- c("id",slug_id,i_("slug",lang()),i_("fecha",lang()), i_("pais",lang()), i_("valor",lang()))
      }

        # print("dta taable")
        # print(dta)
        # print(names(dta))
        # return()
    # <- as.data.frame(oxfam_6$en[as.vector(indicador$indicador[i])])
    #
    # if(ncol(t) == 9) {
    #
    #   colnames(t) <-  c("id", "slug", "slug_en","fecha", "pais_es", "pais_en", "pais_pt","valor","unidad")
    # }
    # else{
    #   if(ncol(t) == 11) {
    #
    #     colnames(t) <-  c("id", "slug", "slug_en","fecha", "pais_es", "pais_en", "pais_pt","valor","unidad_id","unidad","fecha_ct")
    #   }
    #   else  {
    #     colnames(t) <-  c("id", "slug", "slug_en","fecha", "pais_es", "pais_en", "pais_pt","valor")
    #   }
      }
    }
    # if(actual_but$active %in% c("linea","scatter"))   names(data_) = i_(c("pais","fecha", trad),lang=lang())
    # if(actual_but$active %in% c("treemap","mapa","barras"))   names(data_result) = i_(c("pais", trad),lang=lang())
    # if(actual_but$active %in% c("sankey")  & Indicador$value  == "covid_vaccine_agreements")   names(data_result) = i_(c("pais","fabricante", trad),lang=lang())
    # if(actual_but$active %in% c("sankey")  & Indicador$value  == "doses_delivered_vaccine_donations")   names(data_result) = i_(c("pais","pais_donante", trad),lang=lang())
    # if(actual_but$active %in% c("sankey")  & Indicador$value  == "geopolitics_vaccine_donations")   names(data_result) = i_(c("pais","unidad", trad),lang=lang())
    #
     print("DUplicate")

     print(dta |> select(id) |> group_by(id) |> summarise(count_id =n()) |> arrange(desc(count_id)))
     print(dta |> filter(id=="02d84de74fc5234771e2d160fb760f8aa38b132e"))
    #return()


    dta
  })

  possible_viz <- reactive({
    req(quest_choose())
    req(quest_choose_sub())
    #req(Indicador$value)
    question <- quest_choose()
    subquestion <- quest_choose_sub()

    v <- c("mapa", "linea", "barras", "treemap", "scatter", "sankey", "table")

    if(lang()=="en")
      viz <- questions_dash_6 |> filter(pregunta_en %in% question & subpregunta_en %in% subquestion ) |>  select(viz) |> as.vector()
    if(lang()=="es")
      viz <- questions_dash_6 |> filter(pregunta_es %in% question & subpergunta_es %in% subquestion ) |>  select(viz) |> as.vector()
    if(lang()=="pt")
      viz <- questions_dash_6 |> filter(pregunta_pt %in% question & subpregunta_pt %in% subquestion ) |>  select(viz) |> as.vector()
    print("vizzzzzzzzzzzzzzz")
    print(question)
    print(subquestion)
    print(viz)

     # ques <- "What has been the behavior of COVID-19 deaths in the countries between 2020 and 2022?"
     # subques <- "What is the difference between new deaths and new cases per million in each country between 2020 and 2022?"



    # ques <- "¿Qué donaciones y apoyos financieros han obtenido los países para responder a la pandemia?"
    # subques <-"¿Cuánto apoyo financiero en dólares recibieron los países por parte de la Alianza para las Vacunas Gavi y el Banco Mundial?"
    #   viz2 <- questions_dash_6 |> filter(pregunta_es %in% ques & subpergunta_es %in% subques ) |>  select(viz) |> as.vector()



    #  viz2$viz[1]
    #  viz2$viz = viz2$viz[1]
    #  viz2  <- unique(unlist(strsplit(viz2$viz,",")))
    #
    #  viz2[viz2 == "scatter_plot"] = "scatter"
    #  viz2$viz[viz2$viz == "barras_agrupadas"] = "barras"
    #
    #  print("viz")
    # print(viz2)



    ####################################### Static update!!! #############

    print("Possibleeeeee1")
    print(Indicador$value)
    if(!is.null(Indicador$value)){
        if( ("new_deaths_per_million" %in%  as.vector(Indicador$value$indicador) &
             "new_cases_per_million" %in%  as.vector(Indicador$value$indicador)) |
            ("doses_delivered_vaccine_donations"%in%  as.vector(Indicador$value$indicador) &
             "covid_vaccine_agreements" %in%  as.vector(Indicador$value$indicador))
        ){

          viz$viz <- viz$viz[1]
          viz  <- unique(unlist(strsplit(viz$viz,",")))
          print("viz prefinal")
          print(viz)
          viz[viz == "scatter_plot"] <- "scatter"
          viz[viz == "barras_agrupadas"] <- "barras"

          viz[viz == "barras"] <- "linea"
          print("viz final")
          print(viz)

          viz <- unique(viz)
          print(viz)

        }

      else{
        viz$viz[viz$viz == "scatter_plot"] = "scatter"
        viz$viz[viz$viz == "barras_agrupadas"] = "barras"
        print( viz$viz )

        viz  <- unique(unlist(strsplit(viz$viz,",")))
        viz[viz == "treeemap"] = "treemap"
        print("vizz else")
        print(viz)

      }
    }



    ######################################


    v <- intersect(v,viz)





    v <- c(v,"table")

    v
  })


  output$viz_icons <- renderUI({

    req(possible_viz())
    possible_vizt <- possible_viz()

    if(is.null(actual_but$active)) actual <- possible_viz()[1]
    else actual <- actual_but$active
    #
    shinyinvoer::buttonImageInput('viz_selection',
                                  " ",#div(class="title-data-select", "Selecciona tipo de visualización"),
                                  images = possible_vizt,
                                  path = "www/img/",
                                  active = possible_vizt[1],
                                  imageStyle = list(shadow = TRUE,
                                                    borderColor = "#ffffff",
                                                    padding = "3px"))
  })

  output$downloads <- renderUI({

    if (is.null(actual_but$active)) return()
    if (actual_but$active != "table") {
      dsmodules::downloadImageUI( "download_viz",
                                  dropdownLabel =i_("download",lang=lang()),
                                  formats = c("jpeg", "pdf", "png", "html"),
                                  display = "dropdown",
                                  text = i_("download",lang=lang()))
    } else {
      dsmodules::downloadTableUI("dropdown_table",
                                 dropdownLabel = i_("download",lang=lang()),
                                 formats = c("csv", "xlsx", "json"),
                                 display = "dropdown", text ="download")# i_("download",lang=lang()))
    }
  })



  observe({
    dsmodules::downloadTableServer("dropdown_table",
                                   element = data_prep(),
                                   formats = c("csv", "xlsx", "json"))
    dsmodules::downloadImageServer("download_viz",
                                   element = viz_down(),
                                   lib = "highcharter",
                                   formats = c("jpeg", "pdf", "png", "html"),
                                   file_prefix = "plot")
  })

  observe({

   if (!is.null(input$viz_selection)){
        req(possible_viz())

        viz_rec <- possible_viz()
        if (input$viz_selection %in% viz_rec) {
          actual_but$active <- input$viz_selection
        } else {
          actual_but$active <- viz_rec[1]
        }
   }
    else{
     req(possible_viz())
     viz_rec <- possible_viz()

     actual_but$active <- viz_rec[1]
   }

  })
  #####################################

  data_viz <- reactive({

    req(data_prep())
    req(actual_but$active)
    dta <- data_prep()
    Unidad$value <-NULL
    title_x_axis$value <- NULL
    title_y_axis$value <- NULL
    trad <- "mean"
    title_y_axis$value <-  i_(trad,lang=lang())
    var_calc <- "valor"


    # print(colnames(dta))
    # print(dta |> head(1))
    # dta |> head(1)
    # print(unique(dta$unidad))
    # return()

    if(lang()=="es" | lang()=="pt")  {
      dta <- as.data.frame(bind_rows(dta))
    }

    group_var <- "pais"
    title_x_axis$value  <- i_("pais",lang=lang())

    if(lang() == "es"){

        if(actual_but$active %in% c("mapa")){
          dta <-   dta |> select(!c(pais_es,pais_pt)) |> rename(pais = pais_en,slug_en = slug_es)
          group_var = "pais"

        }
        else {

             dta <-  dta |> select(!c(pais_en,pais_pt)) |> rename(pais = pais_es,slug_en = slug_es)
        }
    }
    if(lang() == "en"){


       dta <-   dta |> select(!c(pais_es,pais_pt)) |> rename(pais = pais_en)


    }
    if(lang( )== "pt"){

        if(actual_but$active %in% c("mapa")){
          dta <-   dta |> select(!c(pais_es,pais_pt)) |> rename(pais = pais_en)
          group_var = "pais"

        }
        else {

          dta <-   dta |> select(!c(pais_es,pais_en)) |> rename(pais = pais_pt,slug_en = slug_pt)
        }


    }

#    var <- slug_translate |> filter(slug_en %in% input$Indicator)  |> select(slug)


    if(nrow( Indicador$value)==1){

        if(actual_but$active %in% c("linea","scatter")){
          group_var = c("pais","fecha")
          title_x_axis$value <- i_("fecha",lang=lang())

        }
        if(actual_but$active %in% c("mapa","barras","treemap"))  group_var = "pais"
        if(actual_but$active %in% c("barras")){
          title_y_axis$value <- i_("mean",lang=lang())
          title_x_axis$value <- i_("pais",lang=lang())
        }
        if(actual_but$active %in% c("sankey") & Indicador$value  == "covid_vaccine_agreements")  group_var = c("pais","fabrica")
        if(actual_but$active %in% c("sankey") & Indicador$value  == "doses_delivered_vaccine_donations")  group_var = c("pais","donante")
        if(actual_but$active %in% c("sankey") & Indicador$value  == "geopolitics_vaccine_donations")  group_var = c("pais","unidad")
        if(actual_but$active %in% c("barras") &  Indicador$value  == "school_closures"){
          group_var = c("unidad","pais")
          trad <- "count"
          title_y_axis$value <-  i_(trad,lang=lang())
        }

        if(actual_but$active %in% c("barras") &  Indicador$value == "product_pipeline"){
          group_var = c("pais","unidad")
          trad <- "count"
          title_y_axis$value <-  i_(trad,lang=lang())
        }


        if(actual_but$active %in% c("mapa") &  Indicador$value == "product_pipeline"){
          group_var = c("pais")
          trad <- "count"

        }
        if(actual_but$active %in% c("linea") &  Indicador$value  == "school_closures") {
          group_var = c("unidad","fecha")
          #req(input$country)

          filtro <- input$country
          if(is.null(input$country)) {
            filtro <- sel_country()[1]
          }
          trad <- "count"
          title_y_axis$value <-  i_(trad,lang=lang())
          dta <-  dta |> filter(pais %in% filtro)
          dta$fecha <-  format(as.Date(dta$fecha), "%Y-%m")
          print(dta$fecha)
        }


        if( Indicador$value  == "covid_vaccine_agreements" ) {

          # a |> tidyr::pivot_wider(names_from=unidad)
          dta <- dta |> group_by(id, slug, slug_en, fecha, pais, valor) |> mutate(unidadp= paste0(unidad, collapse = "-")) |> tidyr::separate(unidadp,sep="-",into=c("fabrica","vacuna"))


        }

        if( Indicador$value  == "doses_delivered_vaccine_donations" ) {

          dta <- dta |> group_by(id, slug, slug_en, fecha, pais, valor) |> mutate(unidadp= paste0(unidad, collapse = "&")) |> tidyr::separate(unidadp,sep="&",into=c("donante","vacuna"))


        }

        if( Indicador$value  == "geopolitics_vaccine_donations" ) {

          dta <- dta |> group_by(id, slug, slug_en, fecha, pais, valor) |> mutate(unidadp= paste0(unidad, collapse = "&")) |> tidyr::separate(unidadp,sep="&",into=c("unidad","vacuna"))


        }

        ###############################################################################################
        ################################################################################################
        #New section if else
        #no agregation
        #TODO global vector
        line_value_vector <-  c("total_boosters_per_hundred","people_vaccinated_per_hundred","new_cases_per_million", "icu_patients_per_million",
                                "reproduction_rate", "new_test_per_thousand", "positive_rate","tests_per_case", " new_deaths_per_million", "excess_mortality_cumulative" )


        if(actual_but$active %in% c("linea") &  Indicador$value %in%  line_value_vector) {

          data_result <- dta |> select(fecha,valor)
        }
        else{

          if( Indicador$value %in%  line_value_vector) {
            trad <- "mean"
          }



          if( "interagency_response_plan_numinneed" %in% c(Indicador$value)) {

            group_var <- c("pais","fecha")
            trad <- "mean"
            title_y_axis$value <-  i_(trad,lang=lang())
            title_x_axis$value <-  i_("fecha",lang=lang())
          }

          if( "immunization_campaigns" %in% c(Indicador$value) ) {
            trad <- "sum"

          }



          if( "worldbank_gavi_vaccine_financing" %in% c(Indicador$value) ) {
            trad <- "sum"
            title_y_axis$value <-  i_(trad,lang=lang())

          }
          if( "covid_vaccine_agreements" %in% c(Indicador$value)) {
            trad <- "sum"
          }

          if("doses_delivered_vaccine_donations"  %in% c(Indicador$value) & actual_but$active %in% c("sankey")){

            dta <- dta |> group_by(id, slug, slug_en, fecha, pais, valor) |> mutate(unidadp= paste0(unidad, collapse = "&")) |> tidyr::separate(unidadp,sep="&",into=c("unidad","vacuna"))



               group_var <-c("donante","vacuna","pais")
               dta <- dta |> select("donante","vacuna","pais","valor") |> tidyr::pivot_longer(!c("donante","vacuna","pais"),)
          }


          if("doses_delivered_vaccine_donations"  %in% c(Indicador$value) & actual_but$active %in% c("mapa","lineas","barras","treemap")){

           trad= "sum"
          }

          if(ncol(dta)>8) dta <- dta |> select(!unidad) |> distinct()
          print("dta")
          print(dta |> head(1))

          data_result <- var_aggregation(data = dta,
                                         # dic = dic,
                                         agg =trad,
                                         to_agg = var_calc,
                                         name =trad,
                                         group_var =group_var)



          if(actual_but$active %in% c("barras") &  Indicador$value == "product_pipeline"){
            data_result$unidad <- as.factor( data_result$unidad )
            data_result$pais <- as.factor( data_result$pais )
          }
          # print("out")
          if(actual_but$active %in% c("mapa")){
            # print("in")
            #print(colnames(sel_country_lang()))
            #print()

            data_result <- data_result |> left_join(sel_country_lang(), by=c("pais"="pais_en"))
          }

          # if(actual_but$active %in% c("mapa")){
          #   data_result$pais_l <- data_result_pais
          #   if(lang( )== "es"){
          #       paises <- unique(dta$pais_es)
          #       paises_en <- unique(dta$paises_en)
          #       data_result |> select(mutate case_when())
          #
          #      }


        }


        ################################################################################################

    }
    else{

      if(length(unique(dta$unidad))==1){

        group_var = c("pais","slug_en")

        if(ncol(dta)>8) dta <- dta |> select(!unidad) |> distinct()
         data_result <- var_aggregation(data = dta,
                                       # dic = dic,
                                       agg =trad,
                                       to_agg = var_calc,
                                       name =trad,
                                       group_var =group_var)

         title_x_axis$value <- i_("slug",lang=lang())
         title_y_axis$value <- i_(trad,lang=lang())

      }
      else {

        if(actual_but$active  %in% c("scatter")){

          if( ("new_deaths_per_million" %in%  as.vector(Indicador$value$indicador) &
              "new_cases_per_million" %in%  as.vector(Indicador$value$indicador)) |
              ("doses_delivered_vaccine_donations"%in%  as.vector(Indicador$value$indicador) &
               "covid_vaccine_agreements" %in%  as.vector(Indicador$value$indicador))
              ){

            print("into scaterr")

            trad="mean"
            #dta$fecha <-  format(as.Date(dta$fecha), "%Y")

            dta2 <- dta |> select("slug","fecha","valor") |> distinct()
            #dta2 <- dta |> tidyr::pivot_wider(names_from=slug,values_from=valor)
            group_var <- c("slug","fecha")
            title_x_axis$value <- i_("fecha",lang=lang())
            title_y_axis$value <- i_(trad,lang=lang())
            #  var_calc <- c(Indicador$value[1,1][1]$indicador,Indicador$value[2,1][1]$indicador)



            data_result  <- var_aggregation(data = dta2,
                                            # dic = dic,
                                            agg =trad,
                                            to_agg = "valor" ,
                                            name =trad,
                                            group_var =group_var)

              data_result <-  dta2
           # data_result <-  data_result1 |> left_join(data_result2, by = c("fecha"="fecha"))
            # data_result$..labels <- ""
            data_result

             }
          else {
                group_var = c("slug_en","fecha")

                title_x_axis$value <- i_("fecha",lang=lang())
                title_y_axis$value <- i_(trad,lang=lang())

                if(ncol(dta)>8) dta <- dta |> select(!unidad) |> distinct()
                data_result <- var_aggregation(data = dta,
                                               # dic = dic,
                                               agg =trad,
                                               to_agg = var_calc,
                                               name =trad,
                                               group_var =group_var)

                data_result$slug_en <- as.factor( data_result$slug_en)
                data_result <- data_result |> rename(slug = slug_en)



          }

          if(actual_but$active %in% c("treemap","mapa","barras","scatter"))   names(data_result) = c(i_("slug",lang = lang()) ,i_("fecha",lang = lang()), i_(trad, lang=lang()))

         }

        else {

          if(actual_but$active  %in% c("lineas")){ #disable temp

            if( ("new_deaths_per_million" %in%  as.vector(Indicador$value$indicador) &
                 "new_cases_per_million" %in%  as.vector(Indicador$value$indicador)) |
                ("doses_delivered_vaccine_donations"%in%  as.vector(Indicador$value$indicador) &
                 "covid_vaccine_agreements" %in%  as.vector(Indicador$value$indicador))
            ){

              #trad="mean"
              dta$fecha <-  format(as.Date(dta$fecha), "%Y-%m")
              dta$valor <- as.numeric(unlist( dta$valor))

              dta2 <- dta |> select("fecha","slug","valor") |> distinct()
              print("dta2 |> head(1)")
              print(dta2 |> head(1))
              write.csv(dta2, "dta2.csv")
              dta2 <- dta2 |> tidyr::pivot_wider(names_from=slug,values_from=valor)

              #group_var <- c("fecha")
              title_x_axis$value <- i_("valor",lang=lang())
              title_y_axis$value <- i_("fecha",lang=lang())
              #  var_calc <- c(Indicador$value[1,1][1]$indicador,Indicador$value[2,1][1]$indicador)

              colnames(dta2) <- c("fecha",Indicador$value[1,1][1]$indicador, Indicador$value[2,1][1]$indicador)
              data_result <- dta2
              print(data_result[2])
             # data_result <- as.data.frame(bind_rows( data_result ))

              # data_result[Indicador$value[1,1][1]$indicador] <- as.numeric(unlist( data_result[Indicador$value[1,1][1]$indicador]))
              # data_result[Indicador$value[2,1][1]$indicador] <- as.numeric(unlist( data_result[Indicador$value[2,1][1]$indicador]))

               # data_result1 <-
              # data_result1 <- var_aggregation(data = dta2,
              #                                 # dic = dic,
              #                                 agg =trad,
              #                                 to_agg = Indicador$value[1,1][1]$indicador,
              #                                 name =trad,
              #                                 group_var =group_var)
              #
              # data_result2 <- var_aggregation(data = dta2,
              #                                 # dic = dic,
              #                                 agg =trad,
              #                                 to_agg = Indicador$value[2,1][1]$indicador,
              #                                 name =trad,
              #                                 group_var =group_var)
              #
              #
              # data_result <-  data_result1 |> left_join(data_result2, by = c("fecha"="fecha"))
              # # data_result$..labels <- ""

              }


          }
          if(actual_but$active  %in% c("barras", "linea")){

                trad="mean"
                dta$fecha <-  format(as.Date(dta$fecha), "%Y-%m")
                dta2 <- dta |> tidyr::pivot_wider(names_from=slug,values_from=valor)
                group_var <- c("fecha")
                title_x_axis$value <- "" # i_(trad,lang=lang())
                title_y_axis$value <- ""## i_("fecha",lang=lang())
              #  var_calc <- c(Indicador$value[1,1][1]$indicador,Indicador$value[2,1][1]$indicador)


                data_result1 <- var_aggregation(data = dta2,
                                               # dic = dic,
                                               agg =trad,
                                               to_agg = Indicador$value[1,1][1]$indicador,
                                               name =trad,
                                               group_var =group_var)

                data_result2 <- var_aggregation(data = dta2,
                                                # dic = dic,
                                                agg =trad,
                                                to_agg = Indicador$value[2,1][1]$indicador,
                                                name =trad,
                                                group_var =group_var)


               data_result <-  data_result1 |> left_join(data_result2, by = c("fecha"="fecha"))
             }
        }
      data_result
       }

    }

  ##################### translate code
    if(nrow( Indicador$value)==1){

        if(actual_but$active %in% c("sankey")  & Indicador$value  == "covid_vaccine_agreements")   names(data_result) = i_(c("pais","fabricante", trad),lang=lang())
        if(actual_but$active %in% c("sankey")  & Indicador$value  == "doses_delivered_vaccine_donations")   names(data_result) = i_(c("pais","pais_donante", trad),lang=lang())
        if(actual_but$active %in% c("sankey")  & Indicador$value  == "geopolitics_vaccine_donations")   names(data_result) = i_(c("pais","unidad", trad),lang=lang())
        #no agregation
        if(actual_but$active %in% c("linea") &  Indicador$value %in% c("total_boosters_per_hundred","people_vaccinated_per_hundred","new_cases_per_million", "icu_patients_per_million",
                                                                       "reproduction_rate", "new_test_per_thousand", "positive_rate","tests_per_case")) {
          names(data_result) = i_(c("fecha","valor"),lang=lang())
        }else {

        if(actual_but$active %in% c("linea","scatter"))   names(data_result) = i_(c("pais","fecha", trad),lang=lang())
        }
         if(actual_but$active %in% c("treemap","barras"))   names(data_result) = i_(c("pais", trad),lang=lang())
        if(actual_but$active %in% c("mapa"))   names(data_result) = i_(c("pais", trad,"pais_lang"),lang=lang())

        if(actual_but$active %in% c("barras") & Indicador$value  == "school_closures"){
          names(data_result) = i_(c("unidad","pais",trad),lang=lang())
        }
        if(actual_but$active %in% c("linea") & Indicador$value  == "school_closures"){
          names(data_result) = i_(c("unidad","fecha",trad),lang=lang())
        }
        if(actual_but$active %in% c("mapa","barras") &  Indicador$value == "product_pipeline"){
          names(data_result) = i_(c("pais","unidad",trad),lang=lang())

        }

        if( "interagency_response_plan_numinneed" %in% c(Indicador$value)) {
          names(data_result) = c(i_("pais", lang()), i_("fecha", lang()),i_(trad,lang()))

          group_var <- c("pais","fecha")
        }

        if( "worldbank_gavi_vaccine_financing" %in% c(Indicador$value) ) {
          print("worddddddddddddddddddddddddl")
          title_y_axis$value <-  i_(trad,lang=lang())

        }


    }
    else{
      Unidad$value <-length(unique(dta$unidad))
      if(length(unique(dta$unidad))==1){
        if(actual_but$active %in% c("treemap","mapa","barras","linea"))   names(data_result) = i_(c("pais", "indicador",trad),lang=lang())
      }
      else{
        print("valueeeeee")
        Unidad$value <-length(unique(dta$unidad))
        print(Unidad$value)
        print(dta |> filter(slug == Indicador$value[1,1][1]$indicador) |> head(1))
        indicador1 <- dta |> filter(slug == Indicador$value[1,1][1]$indicador) |> select(slug_en) |> distinct()
        indicador2 <- dta |> filter(slug == Indicador$value[2,1][1]$indicador) |> select(slug_en)  |> distinct()


        if(actual_but$active %in% c("treemap","mapa","barras"))   names(data_result) = c(i_("fecha", lang()),indicador1$slug_en,indicador2$slug_en)
        if( actual_but$active %in% c("linea") & "new_deaths_per_million" %in%  as.vector(Indicador$value$indicador) & "new_cases_per_million" %in%  as.vector(Indicador$value$indicador)){

          names(data_result) = c(i_("fecha", lang()),indicador1$slug_en,indicador2$slug_en, "..labels")
        }



      }
    }


    data_result
  })
  ###############calendar pending




  selecting_viz_typeGraph <- function(df, type_viz, param=NULL) {
    # Vizualizaciones requeridas:Clorepethc  Line  Bar   treemap   table, se pueden dejar en un solo if las que no necesitan desagregacion
    prex <- "DatNum"
    if(type_viz=="scatter") {
      prex <-  "CatDatNum"
      # if(!is.null( Unidad$value )){
      #
      #   if( Unidad$value >=2)  prex <- "DatNumNum"
      # }
      }
    if(type_viz=="mapa") {  prex <- "GnmNum" }
    if(type_viz=="linea") {
      # if(ncol(df) > 2)
      print("linnnnnnnnnnnnnnnnnnnn")
      prex <- "CatDatNum"
      print("uni")
      print(Unidad$value)
      if(!is.null( Unidad$value )){

        if( Unidad$value >=2)  prex <- "DatNumNum"
      }
      #TODO global vector
      else{
            line_value_vector <-  c("total_boosters_per_hundred","people_vaccinated_per_hundred","new_cases_per_million", "icu_patients_per_million",
                                    "reproduction_rate", "new_test_per_thousand", "positive_rate","tests_per_case", " new_deaths_per_million", "excess_mortality_cumulative" )

            if( Indicador$value %in% line_value_vector ) {
              prex <-  "DatNum"
            }
      }
      # if(Indicador$value  == "school_closures"){
      #   prex <- "CatCatNum"
      # }


    }
    if(type_viz=="barras" ) {

      prex <- "CatCatNum"
      print("Indicador$value")
      print(Indicador$value)
      if(!is.null(  Indicador$value )){
          if(nrow( Indicador$value)==1)  {
              prex <- "CatNum"
             if( "interagency_response_plan_numinneed" %in% c(Indicador$value)) prex <- "CatCatNum"
          }
          else prex <- "CatCatNum"
          #
          if( "school_closures" %in% Indicador$value  | "product_pipeline" %in% Indicador$value ){
            prex <- "CatCatNum"
          }
          #
          if(!is.null( Unidad$value )){
            print("nrow(Unidad$value)")
            print(Unidad$value)
            if(Unidad$value == 2)  prex <- "CatNumNum"
          }
      }

    }
    if(type_viz=="treemap") {
      prex <- "CatNum"


    }
    if(type_viz=="sankey"){
      if("doses_delivered_vaccine_donations"  %in% c(Indicador$value)){
        prex<-CatCatCat
      }
    }
    print("Prex")

    print(prex)
    prex
  }


  vizFrtype <- reactive({
    req(actual_but$active)
    req(data_viz())
    selecting_viz_typeGraph(data_viz(),actual_but$active)
  })



  viz_opts <- reactive({
   #tryCatch({
      req(data_viz())
      req(actual_but$active)
      myFunc <- NULL
     # var_indicador <- indicador_title$value  |> summarise(slug <- paste(slug,collapse = "-")) |> select(slug)
     # print(var_indicador)
      data_v <- as.data.frame(data_viz())

      indicator_temp <- apply(indicador_title$value, 2, function(y) paste(y, collapse = " <BR> "))
      print("sL")
          print( indicator_temp[[1]])

          if( "new_deaths_per_million" %in%  as.vector(Indicador$value$indicador) & "new_cases_per_million" %in%  as.vector(Indicador$value$indicador)){
            # a <- i_("fecha", lang())
            # b <- i_("indicador",lang())
            # names(data_v)  <- c("fecha", Indicador$value$indicador[1],Indicador$value$indicador[2])
            # a_ext <- paste("<b>",": </b> {fecha} <BR>")
            # b_ext_1 <- paste("<b>",": </b>{",Indicador$value$indicador[1],"} <BR>", sep = "")
            # b_ext_2<- paste("<b>",": </b>{",Indicador$value$indicador[2],"} <BR>", sep = "")
            # toolp <- paste(a,a_ext,b,b_ext_1,b,b_ext_2)

              }

      opts <- list(
        data = data_v,
        marker_radius = 0,
        text_family = "IBM Plex Sans",
        legend_family = "IBM Plex Sans",
        text_color = "#0F1116",
        background_color = "#FFFFFF",
        grid_x_width = 0,
        axis_line_y_size = 1,
        axis_line_x_size = 1,
        axis_line_color = "#CECECE",
        palette_colors = c("#47BAA6", "#151E42", "#FF4824", "#FFCF06", "#FBCFA4", "#FF3D95", "#B13168"),
        title =  indicator_temp[[1]],
        title_align = "center",
        tiltle_size = 15,
        hor_title=   title_x_axis$value,
        ver_title=title_y_axis$value
      )
      if (actual_but$active == "mapa") {
        # opts$legend_title <- input$InsId_rb
        # opts$legend_color <-  "Black"
        # opts$map_bins <- 3
        # opts$map_color_scale <- "Bins"
        # opts$na_color <- "transparent"

        opts$map_name <- "latamcaribbean_countries"
        tooltp <-  paste("<b>",names(data_v[1]),":</b>  {c}</br>","<b>", names(data_v[2]), ":</b> {b}")
        opts$palette_colors <- rev(c("#151E42", "#253E58", "#35606F", "#478388", "#5DA8A2", "#7BCDBE", "#A5F1DF"))
        opts$tooltip_template <- tooltp   # opts$tooltip <- "<b>Country:</b> {Country}<br/><b>Average Price:</b> {mean_show} USD"
        #opts$format_sample_num = "10M"
        # opts$palette_colors <- rev(c("#ef4e00", "#f66a02", "#fb8412", "#fd9d29",
        # "#ffb446", "#ffca6b", "#ffdf98"))
        # opts$palette_colors <- rev(c( "#151E42","#A5F1DF"))
      } else {
        opts$clickFunction <- htmlwidgets::JS(myFunc)
        opts$palette_colors <- "#ef4e00"
        if (actual_but$active == "linea" | actual_but$active == "scatter" ) {

          if( "stringency_index" %in%  as.vector(Indicador$value))  opts$y_max=100
          #opts$marker_enabled <- FALSE
          opts$marker_radius = 0
          opts$palette_colors <- c("#47BAA6", "#151E42", "#FF4824", "#FFCF06",
                                   "#FBCFA4", "#FF3D95","#B13168")

          if( "new_deaths_per_million" %in%  as.vector(Indicador$value$indicador) & "new_cases_per_million" %in%  as.vector(Indicador$value$indicador)){
            # a <- i_("fecha", lang())
            # b <- i_("slug",lang())
            # # names(data_v)  <- c("fecha", Indicador$value$indicador[1],Indicador$value$indicador[2])
            # # a_ext <- paste("<b>",": </b> {fecha} <BR>")
            # # b_ext_1 <- paste("<b>",": </b>{",Indicador$value$indicador[1],"} <BR>", sep = "")
            # # b_ext_2<- paste("<b>",": </b>{",Indicador$value$indicador[2],"} <BR>", sep = "")
            # # tooltp <- paste(a,a_ext,b,b_ext_1,b,b_ext_2)
            opts$tooltip_template <- NULL

          }

          # opts$ver_title <- "Tender Year"
         # opts$tooltip <- paste(paste("{pais_",lang(),"}"), " {Total")

          # opts$hor_title <- stringr::str_to_sentence(input$InsId_rb)
          #opts$format_sample_num = "10M"
          # opts$tooltip <- "<b>Country:</b> {Country}<br/><b>Tender Year:</b> {Tender Year}<br/><b>Average Price:</b> {mean_show} USD"
        }
      }

      if (actual_but$active == "treemap") {
        # opts$Labels <-  sitools::f2si(data_v$mean)

        opts$dataLabels_align <- "middle"
        opts$dataLabels_inside <- TRUE
        opts$dataLabels_show <- TRUE
        opts$legend_show <- FALSE
        opts$palette_colors <- c("#47BAA6", "#151E42", "#FF4824", "#FFCF06",
                                 "#FBCFA4", "#FF3D95","#B13168")
      #  opts$tooltip <- paste(paste("{pais_",lang(),"}"), " {Total}")



      }

      if (actual_but$active == "barras" | actual_but$active == "sankey" ) {
        opts$palette_colors <- c("#47BAA6", "#151E42", "#FF4824", "#FFCF06",
                                 "#FBCFA4", "#FF3D95","#B13168")
        #opts$ver_title <- ""
       # opts$tooltip <- paste(paste("{pais_",lang(),"}"), " {Total}")
        # opts$hor_title <- stringr::str_to_sentence(input$InsId_rb)
       # opts$format_sample_num = "10M"

      }

      opts
    # },
    # error = function(cond) {
    #   return()
    # })
  })


  ############################### Render




  viz_down  <- reactive({

    req(viz_opts())
    if (is.null(vizFrtype())) return()
    viz=""

    if(actual_but$active == "barras" | actual_but$active == "linea" | actual_but$active == "treemap" | actual_but$active == "scatter") {

      type_hgh <-  actual_but$active
      if(actual_but$active == "barras")  type_hgh <- "bar"
      if(actual_but$active == "linea")  type_hgh <- "line"

      viz <- paste0("hgchmagic::", paste0("hgch_", type_hgh, "_", vizFrtype()))
      library(hgchmagic)
    }

    if(actual_but$active == "mapa") { #TODO update with vizFrtype())
      viz <- paste0("hgchmaps::", "hgch_choropleth_GnmNum")

    }

    if(actual_but$active == "sankey") {
      viz <- "hgchmagic::hgch_sankey_CatCatNum"
      library(hgchmagic)
    }


    # if(actual_but$active == "scatter") { #TODO update with vizFrtype())
    #   viz <- paste0("hgchmagic::", "hgch_scatter")
    #
    # }

    # try({
      do.call(eval(parse(text=viz)),
              viz_opts()
       )
    # })
  })

  output$hgch_viz <- highcharter::renderHighchart({
    # tryCatch({
      req(data_viz())
      req(actual_but$active)
      if (actual_but$active %in% c("table")) return()
      viz_down()
    # },
    # error = function(cond) {
    #   return()
    # })
  })
  #
  output$lflt_viz <- leaflet::renderLeaflet({
    req(actual_but$active)
    if (!actual_but$active %in% c("mapa")) return()
    viz_down()  |>
      leaflet::setView(lng = 0, lat = -5, 1.25)
  })

  output$dt_viz <- reactable::renderReactable({
    req(actual_but$active)
    if (actual_but$active != "table") return()
    req(data_prep())
    df <- data_prep()
    reactable(df,
              showSortIcon = TRUE)
    # dtable <- DT::datatable(df,
    #                         rownames = F,
    #                         selection = 'none',
    #                         options = list(
    #                           scrollX = T,
    #                           fixedColumns = TRUE,
    #                           fixedHeader = TRUE,
    #                           scrollY = "500px"
    #                         ))
    #
    # dtable
  })


  output$viz_view <- renderUI({
   # tryCatch({

      req(actual_but$active)
      viz <- actual_but$active

      # if (viz %in% c("map")) {
      #   req(data_viz())
      #   # if(all(is.na(data_viz()$mean))) return("No information available")
      #
      #
      #   shinycustomloader::withLoader(
      #     leaflet::leafletOutput("lflt_viz", height = 600),
      #     type = "html", loader = "loader4"
      #   )
      if (viz == "table") {
        shinycustomloader::withLoader(
          reactable::reactableOutput("dt_viz"),
          type = "html", loader = "loader4"
        )
      } else {
        req(data_viz())

        # if(all(is.na(data_viz()$mean))) return("No information available")

        shinycustomloader::withLoader(
        highcharter::highchartOutput("hgch_viz", height = 600),
         type = "image", loader = "loading_gris.gif"
        )
      }
    # },
    # error = function(cond) {
    #   return()
    # })
  })


  output$dt_viz <- reactable::renderReactable({
    req(actual_but$active)
    if (actual_but$active != "table") return()
    req(data_prep())
    req(data_table())
    df <- data_table()
    reactable(df,
              showSortIcon = TRUE)
    # dtable <- DT::datatable(df,
    #                         rownames = F,
    #                         selection = 'none',
    #                         options = list(
    #                           scrollX = T,
    #                           fixedColumns = TRUE,
    #                           fixedHeader = TRUE,
    #                           scrollY = "500px"
    #                         ))
    #
    # dtable
  })

  # observe({
  #   print( input$last_click)
  #   print(quest_choose())
  output$debug <- renderPrint({
  # })

    #indicador_title$value
    #oxfam_one
    # input$last_click
    # quest_choose()
  #data_prep() |> head(1)
  # data_viz()
  #  data_table()
   # get_basic_lang_data()

  })







}

shinyApp(ui, server)
