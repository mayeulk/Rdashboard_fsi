library(shiny)
library(shiny.i18n)

ui <- shinyUI(fluidPage(
  titlePanel('shiny.i18n'),
  uiOutput('page_content')
))

translator <- Translator$new(translation_csvs_path = "translations")

server <- shinyServer(function(input, output, session) {
  
  i18n <- reactive({
    selected <- input$selected_language
    if (length(selected) > 0 && selected %in% translator$languages) {
      translator$set_translation_language(selected)
    }
    translator
  })
  
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins,
         col = "darkgray", border = "white",
         main = i18n()$t("Histogram of x"), ylab = i18n()$t("Frequency"))
  })
  
  output$page_content <- renderUI({
    tagList(
      sidebarLayout(
        sidebarPanel(
          selectInput('selected_language',
                      i18n()$t("Change language"),
                      choices = translator$languages,
                      selected = input$selected_language),
          sliderInput("bins",
                      "Number of bins:",
                      min = 1,
                      max = 50,
                      value = 30)
        ),
        mainPanel(
          plotOutput("distPlot"),
          p(i18n()$t("This is description of the plot.")),
          p(i18n()$t("dashboard")),
          p(i18n()$t("version")),
          p(i18n()$t("First variable:")),
          p(i18n()$t("Second variable:")),
          p(i18n()$t("Third variable:")),
          p(i18n()$t("Countries:")),
          p(i18n()$t("Show all countries in histogram, scaterplot matrix and table (bypass list of countries)")),
          p(i18n()$t("Show all countries")),
          p(i18n()$t("bypass list of countries")),
          p(i18n()$t("Year")),
          p(i18n()$t("Year for histogram, radar plot and scaterplot matrix")),
          p(i18n()$t("First year of current period")),
          p(i18n()$t("Last year of current period")),
          p(i18n()$t("Charts and table to display")),
          p(i18n()$t("1st var.")),
          p(i18n()$t("1st/2nd var.")),
          p(i18n()$t("Histogram")),
          p(i18n()$t("Radar1")),
          p(i18n()$t("Radar2")),
          p(i18n()$t("3D")),
          p(i18n()$t("Scaterplot matrix")),
          p(i18n()$t("Table")),
          p(i18n()$t("Show all variables in scaterplot matrix and table")),
          p(i18n()$t("Show all variables")),
          p(i18n()$t("Max. number of bins for histogram")),
          p(i18n()$t("Legend")),
          p(i18n()$t("Scroll to the Table then hover table headings for details on each variables.")),
          p(i18n()$t("Data source")),
          p(i18n()$t("License")),
          p(i18n()$t("Description and units")),
          p(i18n()$t("Index Components")),
          p(i18n()$t("Size of circles (xy-plot with 1st/2nd var.)")),
          p(i18n()$t("Components of the composite index are displayed on the radar plots.")),
          p(i18n()$t("Components of the composite index are displayed on the radar plot."))
          
        )
      )
    )
  })
  
  observeEvent(i18n(), {
    updateSliderInput(session, "bins", label =  i18n()$t("Number of bins:"), value = input$bins)
  })
  
})

shinyApp(ui = ui, server = server)
