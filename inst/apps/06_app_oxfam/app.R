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
  panel(title = ui_("pregunta"),
        id = "controls-style",
        collapse = FALSE,
        can_collapse = FALSE,
        width = 300,
        body = div(

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
        width = 300,
        body = div(

          uiOutput("generalsubFilters")
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

           verbatimTextOutput("debug"),

          uiOutput("viz_view")
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

  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)

  parmesan_lang <- reactive({
    i_(parmesan, lang())
  })


  output_parmesan("controls", parmesan = parmesan_lang,
                  input = input, output = output, session = session,
                  env = environment())



  get_basic_lang_data <- reactive({
    req(quest_choose())
    req(quest_choose_sub())

    # if(is.null(quest_choose())) return()
    # if(is.null(quest_choose_sub())) return()
    temp <-  NULL
    question <- quest_choose()
    subquestion <- quest_choose_sub()
    if(lang()=="en"){

      indicador <- questions_dash_6 |> filter(pregunta_en %in% question & subpregunta_en %in% subquestion ) |>  select(indicador)
      Indicador$value <- indicador
      temp <- plyr::ldply( 1:length(indicador$indicador), function(i){
        t  <- as.data.frame(oxfam_6$en[as.vector(indicador$indicador[i])])
        colnames(t) <-  c("id", "slug", "slug_en","fecha", "pais_es", "pais_en", "pais_pt","valor","unidad")
        temp <- rbind(temp,t)
      })


    }
    else{
      if(lang()=="es"){
        indicador <- questions_dash_6 |> filter(pregunta_es %in% question & subpergunta_es %in% subquestion ) |>  select(indicador)
        Indicador$value <- indicador
        # temp <- indicador
        temp <- lapply( 1:length(indicador$indicador), function(i){
          t  <- as.data.frame(oxfam_6$es[as.vector(indicador$indicador[i])])
          colnames(t) <-  c("id", "slug", "slug_es","fecha", "pais_es", "pais_en", "pais_pt","valor","unidad")
          temp <- rbind(temp,t)
        })
      }
      else{

        if(lang()=="pt"){
          indicador <- questions_dash_6 |> filter(pregunta_pt %in% question & subpregunta_pt %in% subquestion ) |>  select(indicador)
          Indicador$value <- indicador
          var <- slug_translate |> filter(slug_pt %in% input$Indicator)  |> select(slug)
          temp <- lapply( 1:length(indicador$indicador), function(i){
            t  <- as.data.frame(oxfam_one$pt[as.vector(indicador$indicador[i])])
            colnames(t) <-  c("id", "slug", "slug_pt","fecha", "pais_es", "pais_en", "pais_pt","valor","unidad")
            temp <- rbind(temp,t)
          })
        }

      }

    }
    temp
  })

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

  output$generalFilters <- renderUI({
    #req(quest_choose())
    dsapptools:::make_buttons( sel_question(), labels = sel_question(), default_active =   sel_question()[1])
  })

  output$generalsubFilters <- renderUI({
    req(sel_question())
  #  req(quest_choose_sub())

    dsapptools:::make_buttons( sel_subquestion(), labels = sel_subquestion(), class="needed_sub", class_active = "basic_active_sub", default_active =   sel_subquestion()[1])
  })


  seek_dataset_lang_prhase <- function(pharse){
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
  }


  click_viz <- reactiveValues(id = NULL)

  click_sub <- reactiveValues( value = NULL)

  observe({
    click_sub$value <-  input$last_click_sub
  })

  quest_choose <- reactive({
    last_btn <- input$last_click
    click_sub$value <- NULL
    #click_viz$id <- NULL
    if (is.null(last_btn)){
      if(lang() == "es") {
        last_btn <- unique(questions_dash_6$pregunta_es)[1]

      } else {
        if(lang() == "en") {
          last_btn <- unique(questions_dash_6$pregunta_en)[1]
        }
        else { if(lang() == "pt")
          last_btn <- unique(questions_dash_6$pregunta_pt)[1]
        }
      }

    }
    print("choose")
    print(last_btn)

    last_btn

  })

  click_viz_sub <- reactiveValues(id = NULL)



  quest_choose_sub <- reactive({
    req(quest_choose())
    print(input$last_click_sub)

    last_btn <- click_sub$value
   # click_viz_sub$id <- NULL
    if (is.null(last_btn)){
      if (is.null(last_btn)){
        if(lang() == "es") {
          temp_q <-  questions_dash_6 |> filter(pregunta_es == quest_choose()) |> select(subpergunta_es)
          last_btn <- unique(temp_q$subpergunta_es)[1]

        } else {
          if(lang() == "en") {
            temp_q <-  questions_dash_6 |> filter(pregunta_en == quest_choose()) |> select(subpregunta_en)
            print("in")
            print(temp_q)
            last_btn <- unique(temp_q$subpregunta_en)[1]
          }
          else { if(lang() == "pt")
            temp_q <-  questions_dash_6 |> filter(pregunta_pt == quest_choose()) |> select(subpregunta_pt)
            last_btn <- unique(temp_q$subpregunta_pt)[1]
          }
        }

      }

    }
    print("subchoose")
    print( last_btn)
    last_btn

  })

  # observe({
  #   quest_choose_sub
  # })

  sel_subquestion <-reactive({
    req(quest_choose())
    print(quest_choose())

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




  Indicador <- reactiveValues( value = NULL)

  sel_question_url<- reactive({

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
        pregunta_es ==  "¿Cuáles compras, desarrollos y aprobaciones de vacunas han hecho los países?"    ~ 5,
        pregunta_es ==  "¿Qué donaciones y apoyos financieros han obtenido los países para responder a la pandemia?"   ~ 6
      ))
      if(lang()=="en" | is.null(lang())) {
        temp2 <- questions_dash_6 |> filter(q1 %in% temp)  |> select(pregunta_en) |> rename(pregunta =  pregunta_en)
      }
      if(lang()=="es") {
        temp2 <- questions_dash_6 |> filter(q1 %in% temp)  |> select(pregunta_es) |> rename(pregunta =  pregunta_es)
      }
      if(lang()=="pt") {
        temp2 <- questions_dash_6 |> filter(q1 %in% temp) |>   select(pregunta_pt)  |> rename(pregunta =  pregunta_pt)
      }
      i_(temp2$pregunta, lang=lang())
    }

    else{

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


  sel_subquestion_url<- reactive({
    req(quest_choose)
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
        subpergunta_es =="¿Cuántos casos de COVID-19 por millón de habitantes se confirmaron por día?"                                       ~ 7,
        subpergunta_es =="¿Cuántas personas han sido internadas en Unidades de Cuidado Intensivo?"                                               ~ 8,
        subpergunta_es =="¿Cuál es la diferencia entre el número de pacientes en Unidades de Cuidado Intensivo y en hospitales por millón?"        ~ 9,
        subpergunta_es =="¿Cuál es la admisión semanal en hospitales por millón en comparación con la admisión en Unidades de Cuidado Intensivo?"    ~ 10,
        subpergunta_es =="¿Cuántas nuevas infecciones en promedio ha causado un solo individuo infectado de COVID-19 en cada país?"                       ~ 11,
        subpergunta_es =="¿Cuántas pruebas de COVID-19 en promedio se han hecho entre mil habitantes de cada país?"                                      ~ 12,
        subpergunta_es == "¿Cuál es la tasa de pruebas positivas de COVID-19 por país?"                                                                      ~ 13,
        subpergunta_es =="¿Cuánto ha sido el total de pruebas dividido entre el número de casos confirmados por país?"                                       ~ 14,
        subpergunta_es =="¿Cuántas muertes a causa de COVID-19 por millón de habitantes se reportaron en cada país, incluyendo presuntas muertes?"          ~ 15,
        subpergunta_es =="¿Cuál es la diferencia de porcentaje entre el número reportado de muertes semanales o mensuales entre 2020 y 2022 y el número proyectado de muertes para el mismo período con base en años anteriores de los países de América Latina y el Caribe?" ~ 16,
        subpergunta_es =="¿Cuál es la diferencia acumulada entre el número informado de muertes desde el 1 de enero de 2020 y el número proyectado de muertes para el mismo período con base en años anteriores de los países de América Latina y el Caribe?" ~ 17,
        subpergunta_es =="¿Cuántas muertes en exceso hubo en total en los países respecto a las proyectadas en años anteriores?" ~ 18,
        subpergunta_es =="¿Cuál es la correlación entre las nuevas muertes por millón y el acumulado en el exceso de mortalidad?"   ~ 19,
        subpergunta_es =="¿Cuál es la diferencia entre las nuevas muerte y los nuevos casos  por millón en cada país entre 2020 y 2022?" ~ 20,
        subpergunta_es =="¿Cuál fue el Índice de Rigurosidad de los países a lo largo de la pandemia por establecer cierres de escuelas, cierres de lugares de trabajo y prohibiciones de viaje, entre otras medidas?" ~ 21,
        subpergunta_es =="¿Cuántas personas con necesidad de asistencia y protección humanitarias fueron asistidas por el Plan de Respuesta Interagencial de Naciones Unidas en los países?"  ~ 22,
        subpergunta_es =="¿Cuántas personas no fueron vacunadas de otras enfermedades debido al COVID-19?"                                                                                    ~ 23,
        subpergunta_es =="¿Cuál ha sido el estado de operación de las instituciones educativas debido a las medidas de los gobiernos para mitigar la pandemia?"                               ~ 24,
        subpergunta_es =="¿Cuál es el Índice de Seguridad de Salud Global de los países, de acuerdo con su seguridad sanitaria y otras capacidades relacionadas para responder a epidemias y pandemias?" ~ 25,
        subpergunta_es =="¿Cuál es la correlación entre el promedio del Índice de Rigurosidad y el Índice de Seguridad de Salud Global de los países?"                                                    ~ 26,
        subpergunta_es =="¿Cuántas dosis recibieron los países de la región a partir de acuerdos con los fabricantes?"                                                                                        ~ 27,
        subpergunta_es =="¿Cuáles fueron los fabricantes con los que los países establecieron acuerdos para obtener vacunas?"                                                                               ~ 28,
        subpergunta_es =="¿Cuáles son los precios por dosis en los países según las investigaciones periodísticas?"                                                                                         ~ 29,
        subpergunta_es =="¿En qué fase de ensayo clínico están las vacunas en los países de la región?"                                                                                                     ~ 30,
        subpergunta_es =="¿Cuántos proyectos de vacunas contra el COVID-19 desarrollaron algunas instituciones en países de América Latina y el Caribe?"                                                    ~ 31,
        subpergunta_es =="¿Cuánto apoyo financiero en dólares recibieron los países por parte de la Alianza para las Vacunas Gavi y el Banco Mundial?"                                                      ~ 32,
        subpergunta_es =="¿Cuánta asignación de financiación de proyectos en dólares han recibido los países por parte del Fondo Central para la Acción en Casos de Emergencia (CERF) de OCHA?"             ~ 33,
        subpergunta_es =="¿Qué países donaron vacunas a América Latina y el Caribe?"                                                                                                                        ~ 34,
        subpergunta_es =="¿Cuántas dosis donadas recibieron los países de América Latina y el Caribe?"                                                                                                      ~ 35,
        subpergunta_es =="¿Cuántas dosis donaron China y Estados Unidos a los países de la región y cómo están distribuidas?"                                                                               ~ 36,
        subpergunta_es =="¿Cuál es la diferencia entre las dosis obtenidas por acuerdos con fabricantes y por donaciones?"  ~ 37
      ))



      if(lang()=="en" | is.null(lang())) {

        data_t <- questions_dash_6 |> filter(pregunta_en %in% unique(sel_question_url()) )
        temp2 <- data_t |> filter(q2 %in% temp)  |> select(subpregunta_en) |> rename(subpregunta =  subpregunta_en)

      }
      if(lang()=="es") {

        data_t <-questions_dash_6 |> filter(pregunta_es  %in%  sel_question_url() )
        temp2 <- data_t |> filter(q2 %in% temp)  |> select(subpergunta_es) |> rename(subpregunta =  subpergunta_es)

      }
      if(lang()=="pt") {
        data_t <-questions_dash_6 |> filter(pregunta_pt == sel_question_url() )
        temp2 <- data_t |> filter(q2 %in% temp)  |> select(subpregunta_pt) |> rename(subpregunta =  subpregunta_pt)
      }

      temp <- temp2$subpregunta
    }
    else{
      if(lang() == "es") {
        data_t <-questions_dash_6 |> filter(pregunta_es  %in%   unique(questions_dash_6$pregunta_es)[1])
        temp2 <- data_t  |> select(subpergunta_es) |> rename(subpregunta = subpergunta_es)
        temp <- unique(temp2$subpregunta)[1]


      } else {
        if(lang() == "en") {
          data_t <-questions_dash_6 |> filter(pregunta_en  %in%   unique(questions_dash_6$pregunta_en)[1])
          temp2 <- data_t  |> select(subpregunta_en) |> rename(subpregunta =  subpregunta_en)
          temp <- unique(temp2$subpregunta)[1]
        }
        else {
          if(lang() == "pt")
            unique(questions_dash_6$pregunta_pt)[1]
          data_t <-questions_dash_6 |> filter(pregunta_pt  %in%   unique(questions_dash_6$pregunta_pt)[1])
          temp2 <- data_t  |> select(subpregunta_pt) |> rename(subpregunta =  subpregunta_pt)
          temp <- unique(temp2$subpregunta)[1]
        }
      }}


  })




  output$subquestion <- renderUI({


    default_select <- NULL
    default_select <- NULL

    shiny::selectizeInput("subquestion", label= "", choices=  sel_subquestion(), selected = sel_subquestion_url(), multiple =TRUE,
                          options = list(
                            placeholder = "All", plugins=list("remove_button","drag_drop"))
    )




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



  possible_viz <- reactive({
    req(quest_choose())
    req(quest_choose_sub())
    question <- quest_choose()
    subquestion <- quest_choose_sub()
    v <- c("mapa", "linea", "barras", "treemap", "scatter", "sankey", "table")

    if(lang()=="en")
      viz <- questions_dash_6 |> filter(pregunta_en %in% question & subpregunta_en %in% subquestion ) |>  select(viz) |> as.vector()
    if(lang()=="es")
      viz <- questions_dash_6 |> filter(pregunta_es %in% question & subpergunta_es %in% subquestion ) |>  select(viz) |> as.vector()
    if(lang()=="pt")
      viz <- questions_dash_6 |> filter(pregunta_pt %in% question & subpregunta_pt %in% subquestion ) |>  select(viz) |> as.vector()

    viz  <- unlist(strsplit(viz$viz,","))


    v <- intersect(v,viz)
    print("v")
    print(v)
    v <- c(v,"table")
    print(v)
    v
  })

  actual_but <- reactiveValues(active = NULL)
  country_url <-  reactiveValues(paises = NULL)




  output$viz_icons <- renderUI({

    req(possible_viz())
    possible_vizt <- possible_viz()
    print( possible_vizt)
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
                                 dropdownLabel = "download", #i_("download",lang=lang()),
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
     print("entro2")
       print(input$viz_selection)
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
     print("entro")
     print(viz_rec[1])
     actual_but$active <- viz_rec[1]
   }

  })
  #####################################

  data_viz <- reactive({

    req(data_prep())
    req(actual_but$active)
    dta <- data_prep()

    # req(operations())
    # req(input$Operation_rb)


    if(lang()=="es")  dta <- as.data.frame(dta)

    if(lang() == "es"){

      dta <-  dta |> select(!c(pais_en,pais_pt)) |> rename(pais = pais_es,slug_en = slug_es)
    }
    if(lang() == "en"){

      dta <-   dta |> select(!c(pais_es,pais_pt)) |> rename(pais = pais_en)
    }
    if(lang( )== "pt"){

      dta <-   dta |> select(!c(pais_es,pais_en)) |> rename(pais = pais_pt,slug_en = slug_pt)
    }
    # var <- slug_translate |> filter(slug_en %in% input$Indicator)  |> select(slug)
    # var <- "new_vaccinations"
    # var_agg <- slug_agg_one |> filter(slug %in% var) |> select(agg)

    # dta$Year <-  format(as.Date(dta$fecha, format="%d/%m/%Y"),"%Y")
    group_var ="pais"
    dta
    if(actual_but$active %in% c("linea","scatter"))  group_var = c("pais","fecha")
    if(actual_but$active %in% c("mapa","barras","treemap"))  group_var = "pais"
    if(actual_but$active %in% c("sankey") & Indicador$value  == "covid_vaccine_agreements")  group_var = c("pais","fabrica")
    if(actual_but$active %in% c("sankey") & Indicador$value  == "doses_delivered_vaccine_donations")  group_var = c("pais","donante")
    if(actual_but$active %in% c("sankey") & Indicador$value  == "geopolitics_vaccine_donations")  group_var = c("pais","unidad")




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


    # hgch_sankey_CatCatNum(data_result)
    # if(input$Operation_rb %in% c("Total")) trad = "sum"
    # if(input$Operation_rb %in% c("Mean","Promedio","Média")) trad = "mean"
    trad= "sum"
    dta <- dta |> select(!unidad) |> distinct()
    data_result <- var_aggregation(data = dta,
                                   # dic = dic,
                                   agg =trad,
                                   to_agg = "valor",
                                   name =trad,
                                   group_var =group_var)


    if(actual_but$active %in% c("linea","scatter"))   names(data_result) = i_(c("pais","fecha", trad),lang=lang())
    if(actual_but$active %in% c("treemap","mapa","barras"))   names(data_result) = i_(c("pais", trad),lang=lang())
    if(actual_but$active %in% c("sankey")  & Indicador$value  == "covid_vaccine_agreements")   names(data_result) = i_(c("pais","fabricante", trad),lang=lang())
    if(actual_but$active %in% c("sankey")  & Indicador$value  == "doses_delivered_vaccine_donations")   names(data_result) = i_(c("pais","pais_donante", trad),lang=lang())
    if(actual_but$active %in% c("sankey")  & Indicador$value  == "geopolitics_vaccine_donations")   names(data_result) = i_(c("pais","unidad", trad),lang=lang())


    data_result
  })
  ###############calendar pending




  selecting_viz_typeGraph <- function(df, type_viz, param=NULL) {
    # Vizualizaciones requeridas:Clorepethc  Line  Bar   treemap   table, se pueden dejar en un solo if las que no necesitan desagregacion
    prex <- "DatNum"
    if(type_viz=="scatter") {  prex <-  "CatDatNum" }

    if(type_viz=="mapa") {  prex <- "GnmNum" }
    if(type_viz=="linea") {
      # if(ncol(df) > 2)
      prex <- "CatDatNum"
    }
    if(type_viz=="barras" ) {
      prex <- "CatNum"


    }
    if(type_viz=="treemap") {
      prex <- "CatNum"


    }

    prex
  }





  viz_opts <- reactive({
    tryCatch({
      req(data_viz())
      req(actual_but$active)

      myFunc <- NULL

      if (actual_but$active %in% c("treemap")) {
        myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "',{cat:event.point.node, id:event.point.name, timestamp: new Date().getTime()});}");

      }


      if(length(unique(input$Country)) > 1){
        if (!"All" %in% input$Country){
          if (actual_but$active %in% c("treemap")) {
            myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "',{cat:event.point.node, id:event.point.name, timestamp: new Date().getTime()});}");

          }
          if (actual_but$active %in% c("barras","linea")) {
            myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {cat:this.name, id:event.point.category, timestamp: new Date().getTime()});}")
          }
        }
        else {
          if (actual_but$active %in% c( "barras")) {
            myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {id:event.point.name, timestamp: new Date().getTime()});}")

          }
          if (actual_but$active %in% c("linea")) {
            myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {cat:this.name, id:event.point.category, timestamp: new Date().getTime()});}")

          }

          if (actual_but$active %in% c("treemap")) {
            myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "',{cat:event.point.node, id:event.point.name, timestamp: new Date().getTime()});}");

          }


        }
      }
      else {
        if (actual_but$active %in% c( "barras")) {
          myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {id:event.point.name, timestamp: new Date().getTime()});}")
        }
        if (actual_but$active %in% c("linea")) {
          myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {cat:this.name, id:event.point.category, timestamp: new Date().getTime()});}")

        }
        # if (actual_but$active %in% c("treemap")) {
        #   myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "',{cat:event.point.node, id:event.point.name, timestamp: new Date().getTime()});}");
        #
        # }
      }

      data_v <- as.data.frame(data_viz())

      opts <- list(
        data = data_v,
        orientation = "hor",
        ver_title = " ",
        hor_title = " ",
        label_wrap_legend = 100,
        label_wrap = 40,
        background_color = "#ffffff",
        axis_line_y_size = 1,
        axis_line_color = "#dbd9d9",
        grid_y_color = "#dbd9d9",
        grid_x_color = "#fafafa",
        cursor = "pointer",
        map_tiles = "OpenStreetMap",
        legend_position = "bottomleft",
        border_weight = 0.3,
        map_provider_tile = "url",
        map_extra_layout = "https://maps.geoapify.com/v1/tile/osm-bright-smooth/{z}/{x}/{y}.png?apiKey=3ccf9d5f19894b32b502485362c99163",
        map_name_layout = "osm-brigh",
        # format_sample_num = "10M",
        format_numericSymbols = T
      )
      if (actual_but$active == "mapa") {
        # opts$legend_title <- input$InsId_rb
        opts$legend_color <-  "Black"
        opts$map_bins <- 3
        opts$map_color_scale = "Bins"
        opts$na_color <- "transparent"
        opts$map_name <- "latamcaribbean_countries"
        # opts$tooltip <- "<b>Country:</b> {Country}<br/><b>Average Price:</b> {mean_show} USD"
        opts$format_sample_num = "10M"
        # opts$palette_colors <- rev(c("#ef4e00", "#f66a02", "#fb8412", "#fd9d29",
        # "#ffb446", "#ffca6b", "#ffdf98"))
        opts$palette_colors <- rev(c( "#151E42","#A5F1DF"))
      } else {
        opts$clickFunction <- htmlwidgets::JS(myFunc)
        opts$palette_colors <- "#ef4e00"
        if (actual_but$active == "linea" | actual_but$active == "scatter" ) {
          opts$marker_enabled <- FALSE
          opts$palette_colors <- c("#47BAA6", "#151E42", "#FF4824", "#FFCF06",
                                   "#FBCFA4", "#FF3D95","#B13168")
          # opts$ver_title <- "Tender Year"
          # opts$hor_title <- stringr::str_to_sentence(input$InsId_rb)
          opts$format_sample_num = "10M"
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



      }

      if (actual_but$active == "barras") {
        opts$palette_colors <- c("#47BAA6", "#151E42", "#FF4824", "#FFCF06",
                                 "#FBCFA4", "#FF3D95","#B13168")
        opts$ver_title <- "Drug type"
        # opts$hor_title <- stringr::str_to_sentence(input$InsId_rb)
        opts$format_sample_num = "10M"

      }

      opts
    },
    error = function(cond) {
      return()
    })
  })


  ############################### Render

  vizFrtype <- reactive({
    req(actual_but$active)
    req(data_viz())
    selecting_viz_typeGraph(data_viz(),actual_but$active)
  })



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

    try({
      do.call(eval(parse(text=viz)),
              viz_opts()
      )
    })
  })

  output$hgch_viz <- highcharter::renderHighchart({
    tryCatch({
      req(data_viz())
      req(actual_but$active)
      if (actual_but$active %in% c("table")) return()
      viz_down()
    },
    error = function(cond) {
      return()
    })
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
    tryCatch({

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

        #shinycustomloader::withLoader(
        highcharter::highchartOutput("hgch_viz", height = 600)#,
        #   type = "html", loader = "loader4"
        # )
      }
    },
    error = function(cond) {
      return()
    })
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

  # observe({
  #   print( input$last_click)
  #   print(quest_choose())
  # })

  output$debug <- renderPrint({

    #oxfam_one
    # input$last_click
    # quest_choose()
    #data_prep() |> head(1)
    #data_viz()
    #get_basic_lang_data()

  })







}

shinyApp(ui, server)
