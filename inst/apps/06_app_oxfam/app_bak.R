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
  panel(title = ui_("pregunta"),
        id = "controls-style",
        collapse = FALSE,
        can_collapse = FALSE,
        width = 300,
        body = div(

          uiOutput("controls")
        )
  ),
  panel(title = ui_("subpregunta"),
        id = "controls-style2",

        can_collapse = FALSE,
        width = 300,
        body = div(

          uiOutput("subquestion")
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
    if(is.null(input$question)) return()
    if(is.null(input$subquestion)) return()

    temp <-  NULL
    if(lang()=="en"){

      indicador <- questions_dash_6 |> filter(pregunta_en %in% input$question & subpregunta_en %in% input$subquestion ) |>  select(indicador)
      temp <- plyr::ldply( 1:length(indicador$indicador), function(i){
        t  <- as.data.frame(oxfam_6$en[as.vector(indicador$indicador[i])])
        colnames(t) <-  c("id", "slug", "slug_en","fecha", "pais","valor","Unidad")
        temp <- rbind(temp,t)
      })


    }
    else{
      if(lang()=="es"){

        indicador <- questions_dash_6 |> filter(pregunta_es %in% input$question & subpregunta_es%in% input$subquestion ) |>  select(indicador)
        temp <- lapply( 1:length(indicador$indicador), function(i){
          t  <- as.data.frame(oxfam_6$es[as.vector(var$slug[i])])
          colnames(t) <-  c("id", "slug", "slug_en","fecha", "pais","valor","Unidad")
          temp <- rbind(temp,t)
        })
      }
      else{

        if(lang()=="pt"){
          indicador <- questions_dash_6 |> filter(pregunta_pt %in% input$question & subpregunta_pt %in% input$subquestion ) |>  select(indicador)
          var <- slug_translate |> filter(slug_pt %in% input$Indicator)  |> select(slug)
          temp <- lapply( 1:length(indicador$indicador), function(i){
            t  <- as.data.frame(oxfam_one$pt[as.vector(indicador$indicador[i])])
            colnames(t) <-  c("id", "slug", "slug_en","fecha", "pais","valor","Unidad")
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


  sel_subquestion <-reactive({
    req(input$question)

    if(lang() == "es") {
      t <- questions_dash_6 |> dplyr::filter(pregunta_es %in% input$question) |> select(subpergunta_es)
      print(t$subpergunta_es)
      unique(t$subpergunta_es)

    } else {
      if(lang() == "en") {
        t <- questions_dash_6 |> dplyr::filter(pregunta_en %in% input$question) |> select(subpregunta_en)
        print(t$subpregunta_en)
        unique(t$subpregunta_en)
      }
      else { if(lang() == "pt")
        t <- questions_dash_6 |> dplyr::filter(pregunta_pt %in% input$question) |> select(subpregunta_pt)
      print(t$subpregunta_pt)
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



  sel_question_url<- reactive({

    query <- parseQueryString(session$clientData$url_search)
    temp <- stringr::str_to_title(query[["pregunta"]])

  })

  sel_subquestion_url<- reactive({

    query <- parseQueryString(session$clientData$url_search)
    temp <- stringr::str_to_title(query[["subpregunta"]])

  })




  output$subquestion <- renderUI({


    default_select <- NULL
    default_select <- NULL

    shiny::selectizeInput("subquestion", label= "", choices=  sel_subquestion(), selected = sel_subquestion_url(), multiple =TRUE,
                          options = list(
                            placeholder = "All", plugins=list("remove_button","drag_drop"))
    )




  })




  observeEvent(input$hcClicked, {
    if (is.null(data_viz())) return()


    if(actual_but$active %in% c("treemap")) {

      click_viz$id <- input$hcClicked$cat$parent
      click_viz$cat <- input$hcClicked$cat$name


      if(length(unique(input$Country)) > 1){
        if (!"All" %in% input$Country){

          if (!is.null(input$hcClicked$cat$parent)) {
            click_viz$id <-  input$hcClicked$cat$parent
            click_viz$cat <- input$hcClicked$cat$name
          }
          else{
            click_viz$id <- input$hcClicked$id }
        }
        else{

          click_viz$id <- input$hcClicked$id }
      }
      if(length(unique(input$Country)) == 1){
        if (!"All" %in% input$Country){

          if (!is.null(input$hcClicked$cat$parent)) {
            click_viz$id <-   input$hcClicked$cat$parent
            click_viz$cat <-   input$hcClicked$cat$name
          }
          else{   click_viz$id <- input$hcClicked$id }
        }
        else{   click_viz$id <- input$hcClicked$id }
      }
      # else{
      #
      # if(is.null(input$Country)) #print("entrooooo4")
      #
      # }

      # else {   click_viz$id <- NULL }
    }
    else{
      if(!actual_but$active %in% c("line","bar")){
        if (!is.null(input$hcClicked$id)) {
          click_viz$id <- input$hcClicked$id

        }
        else {   click_viz$id <- NULL

        }

      }
      else{
        if (!is.null(input$hcClicked$id)) {
          click_viz$id <- input$hcClicked$id
          click_viz$cat <- input$hcClicked$cat

        }
        else {   click_viz$id <- NULL
        click_viz$cat <- NULL
        }
      }
    }

    #print(click_viz$id)
    #print(click_viz$cat)
  })


  data_prep <- reactive({
    if(is.null(input$question)) return()
    if(is.null(input$subquestion)) return()


    data  <- get_basic_lang_data()
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
    # v <- c("line", "bar")
    # v <- c(v, "table","map")
    v <- c("map", "line", "bar", "treemap", "scatter", "table")
    v
  })

  actual_but <- reactiveValues(active = NULL)
  country_url <-  reactiveValues(paises = NULL)




  output$viz_icons <- renderUI({
    ###########print("icons")
    req(possible_viz())
    possible_viz <- possible_viz()
    #
    shinyinvoer::buttonImageInput('viz_selection',
                                  " ",#div(class="title-data-select", "Selecciona tipo de visualización"),
                                  images = possible_viz,
                                  path = "www/img/",
                                  active = actual_but$active,
                                  imageStyle = list(shadow = TRUE,
                                                    borderColor = "#ffffff",
                                                    padding = "3px"))
  })

  output$downloads <- renderUI({

    if (is.null(actual_but$active)) return()
    if (actual_but$active != "table") {
      dsmodules::downloadImageUI("download_viz",
                                 dropdownLabel ="Download",
                                 formats = c("jpeg", "pdf", "png", "html"),
                                 display = "dropdown",
                                 text = "Download")
    } else {
      dsmodules::downloadTableUI("dropdown_table",
                                 dropdownLabel = "Download",
                                 formats = c("csv", "xlsx", "json"),
                                 display = "dropdown", text = "Download")
    }
  })



  observe({
    dsmodules::downloadTableServer("dropdown_table",
                                   element = data_down(),
                                   formats = c("csv", "xlsx", "json"))
    dsmodules::downloadImageServer("download_viz",
                                   element = viz_down(),
                                   lib = "highcharter",
                                   formats = c("jpeg", "pdf", "png", "html"),
                                   file_prefix = "plot")
  })

  observe({
    if (is.null(input$viz_selection)) return()
    req(possible_viz())
    viz_rec <- possible_viz()
    if (input$viz_selection %in% viz_rec) {
      actual_but$active <- input$viz_selection
    } else {
      actual_but$active <- viz_rec[1]
    }

  })
  #####################################

  data_viz <- reactive({

    req(data_prep())
    # req(operations())
    # req(input$Operation_rb)

    # var <- slug_translate |> filter(slug_en %in% input$Indicator)  |> select(slug)
    # var <- "new_vaccinations"
    # var_agg <- slug_agg_one |> filter(slug %in% var) |> select(agg)
    dta <- data_prep()
    # dta$Year <-  format(as.Date(dta$fecha, format="%d/%m/%Y"),"%Y")
    group_var = "pais"
    if(actual_but$active %in% c("line","scatter"))  group_var = c("pais","fecha")
    if(actual_but$active %in% c("map","bar","treemap"))  group_var = "pais"



    # if(input$Operation_rb %in% c("Total")) trad = "sum"
    # if(input$Operation_rb %in% c("Mean","Promedio","Média")) trad = "mean"
    trad= "sum"

    data_result <- var_aggregation(data = dta,
                                   # dic = dic,
                                   agg =trad,
                                   to_agg = "valor",
                                   name =trad,
                                   group_var =group_var)


    if(actual_but$active %in% c("line","scatter"))   names(data_result) = i_(c("pais","fecha", trad),lang=lang())
    else  names(data_result) = i_(c("pais", trad),lang=lang())
     data_result
  })
  ###############calendar pending




  selecting_viz_typeGraph <- function(df, type_viz, param=NULL) {
    # Vizualizaciones requeridas:Clorepethc  Line  Bar   treemap   table, se pueden dejar en un solo if las que no necesitan desagregacion
    prex <- "DatNum"
    if(type_viz=="scatter") {  prex <-  "CatDatNum" }

    if(type_viz=="map") {  prex <- "GnmNum" }
    if(type_viz=="line") {
      # if(ncol(df) > 2)
      prex <- "CatDatNum"
    }
    if(type_viz=="bar" ) {
      prex <- "CatNum"


    }
    if(type_viz=="treemap") {
      prex <- "CatNum"


    }
    #print(prex)
    prex
  }


  # observe({
  #   datat <-  data_prep()
  #
  #
  #
  # })



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
          if (actual_but$active %in% c("bar","line")) {
            myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {cat:this.name, id:event.point.category, timestamp: new Date().getTime()});}")
          }
        }
        else {
          if (actual_but$active %in% c( "bar")) {
            myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {id:event.point.name, timestamp: new Date().getTime()});}")

          }
          if (actual_but$active %in% c("line")) {
            myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {cat:this.name, id:event.point.category, timestamp: new Date().getTime()});}")

          }

          if (actual_but$active %in% c("treemap")) {
            myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "',{cat:event.point.node, id:event.point.name, timestamp: new Date().getTime()});}");

          }


        }
      }
      else {
        if (actual_but$active %in% c( "bar")) {
          myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {id:event.point.name, timestamp: new Date().getTime()});}")
        }
        if (actual_but$active %in% c("line")) {
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
      if (actual_but$active == "map") {
        # opts$legend_title <- input$InsId_rb
        opts$legend_color <-  "Black"
        opts$map_bins <- 3
        opts$map_color_scale = "Bins"
        opts$na_color <- "transparent"
        # opts$tooltip <- "<b>Country:</b> {Country}<br/><b>Average Price:</b> {mean_show} USD"
        opts$format_sample_num = "10M"
        # opts$palette_colors <- rev(c("#ef4e00", "#f66a02", "#fb8412", "#fd9d29",
        # "#ffb446", "#ffca6b", "#ffdf98"))
        opts$palette_colors <- rev(c( "#151E42","#A5F1DF"))
      } else {
        opts$clickFunction <- htmlwidgets::JS(myFunc)
        opts$palette_colors <- "#ef4e00"
        if (actual_but$active == "line" | actual_but$active == "scatter" ) {
          #print("INNNNNNNNNNNNNNNNNN")
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


        # if(length(unique(input$Country)) >= 1) {
        #   if (!"All" %in% input$Country){
        #     # if(input$sel_check_opt == FALSE){ opts$sort <- "desc" }
        #     opts$tooltip <- "<b>Country:</b> {Country}<br/><b>Drug type:</b> {Drug type}<br/><b>Average Price:</b> {mean_show} USD"
        #     opts$datalabel_formmater_js  <- TRUE
        #
        #     if(length(unique(input$Country)) > 1){
        #       opts$palette_colors <- c("#47BAA6", "#151E42", "#FF4824", "#FFCF06",
        #                                "#FBCFA4", "#FF3D95","#B13168")
        #     }
        #     else{
        #       if(length(unique(input$Country)) == 1){
        #         #TODO, el color no cambia
        #         opts$color_by <- "Drug type"
        #       }
        #     }
        #   }
        #   else{
        #     opts$tooltip <- "<b>Country:</b> {Country}<br/><b>Drug type:</b> {Drug type}<br/><b>Average Price:</b> {mean_show} USD"
        #     opts$datalabel_formmater_js  <- TRUE
        #     opts$color_by <- "Country"
        #
        #   }
        #
        # }
        #
        # else{
        #   opts$tooltip <- "<b>Country:</b> {Country}<br/><b>Drug type:</b> {Drug type}<br/><b>Average Price:</b> {mean_show} USD"
        #   opts$datalabel_formmater_js  <- TRUE
        #
        #   # opts$color_by <- "Country"
        #   opts$palette_colors <- c("#47BAA6", "#151E42", "#FF4824", "#FFCF06",
        #                            "#FBCFA4", "#FF3D95","#B13168")
        # }

      }

      if (actual_but$active == "bar") {
        opts$palette_colors <- c("#47BAA6", "#151E42", "#FF4824", "#FFCF06",
                                 "#FBCFA4", "#FF3D95","#B13168")
        opts$ver_title <- "Drug type"
        # opts$hor_title <- stringr::str_to_sentence(input$InsId_rb)
        opts$format_sample_num = "10M"
        # if(input$sel_check_opt == FALSE){ opts$sort <- "desc" }

        # if(length(unique(input$Country)) > 1){
        #   if (!"All" %in% input$Country){
        #     opts$tooltip <- "<b>Drug type:</b> {Drug type}<br/><b>Country:</b> {Country}<br/><b>Average Price:</b> {mean_show} USD"
        #
        #   }
        #   else{
        #     opts$tooltip <- "<b>Drug type:</b> {Drug type}<br/><b>Average Price:</b> {mean_show} USD"
        #
        #   }
        #
        # }
        #
        # else{
        #   opts$tooltip <- "<b>Drug type:</b> {Drug type}<br/><b>Average Price:</b> {mean_show} USD"
        # }
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

    if(actual_but$active == "bar" | actual_but$active == "line" | actual_but$active == "treemap" | actual_but$active == "scatter") {
      viz <- paste0("hgchmagic::", paste0("hgch_",actual_but$active, "_", vizFrtype()))
      library(hgchmagic)
    }

    if(actual_but$active == "map") { #TODO update with vizFrtype())
      viz <- paste0("hgchmaps::", "hgch_choropleth_GnmNum")

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
    if (!actual_but$active %in% c("map")) return()
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

  output$debug <- renderPrint({

    #oxfam_one
    data_viz()
   # get_basic_lang_data()
    # data_prep()
  })







}

shinyApp(ui, server)
