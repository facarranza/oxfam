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
          verbatimTextOutput("debug")
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
          img(src= 'logos.svg', style = "border-top: 1px solid #252525;",
              align = "left", width = 300, height = 80))
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



  output$debug <- renderPrint({
    oxfam_one
  })



}

shinyApp(ui, server)
